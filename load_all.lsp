; ==================================================
; AutoLispSwift Update v6.5
; Сохранять ТОЛЬКО в кодировке ANSI (Windows-1251)!
; ==================================================

;; =====================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; =====================================================================
(defun split-string (str delim / pos res start)
  (setq start 0 res nil)
  (while (setq pos (vl-string-search delim str start))
    (setq res (cons (substr str (1+ start) (- pos start)) res))
    (setq start (+ pos (strlen delim))))
  (setq res (cons (substr str (1+ start)) res))
  (reverse res))

(defun ensure-directory (path / dir)
  (setq dir (vl-filename-directory path))
  (if (and dir (not (findfile dir)))
    (vl-mkdir dir))
  path)

(defun file-get-contents (path / f content line)
  (if (setq f (open path "r"))
    (progn
      (setq content "")
      (while (setq line (read-line f))
        (setq content (strcat content line "\n")))
      (close f)
      content)
    ""))

;; Лог: выводит только если не тихий режим
(defun swift-log (msg)
  (if (not *swift-silent*) (princ msg)))

;; Дата для имён бэкапов: YYYY-MM-DD
(defun swift-date (/ cd di yr mo dy)
  (setq cd (getvar "CDATE")
        di (fix cd)
        yr (itoa (fix (/ di 10000)))
        mo (substr (itoa (+ 100 (rem (fix (/ di 100)) 100))) 2)
        dy (substr (itoa (+ 100 (rem di 100))) 2))
  (strcat yr "-" mo "-" dy))

;; Копирует файл в backup/ перед заменой (не перезаписывает если бэкап уже есть за сегодня)
(defun backup-file (fullpath backup-dir / nm ext dst)
  (if (findfile fullpath)
    (progn
      (setq nm  (vl-filename-base fullpath)
            ext (vl-filename-extension fullpath)
            dst (strcat backup-dir "\\" nm "_" (swift-date) ext ".bak"))
      (ensure-directory dst)
      (if (not (findfile dst))
        (vl-file-copy fullpath dst)))))

;; =====================================================================
;; LOCK-ФАЙЛ — защита от двойного запуска
;; =====================================================================
(defun swift-lock-acquire (dir / lock-path f)
  (setq lock-path (strcat dir "\\_SWIFT_LOCK.txt"))
  (if (findfile lock-path)
    (progn
      ;; Проверяем возраст: если lock старше 15 минут — считаем зависшим
      (setq lock-age (vl-catch-all-apply 'atof
                       (list (vl-string-trim " \t\r\n"
                               (file-get-contents lock-path)))))
      (if (and (not (vl-catch-all-error-p lock-age))
               (> (- (getvar "DATE") lock-age) 0.0105))
        (progn
          (princ "\n⚠ Найден устаревший lock-файл — сбрасываем")
          (vl-file-delete lock-path)
          (if (setq f (open lock-path "w"))
            (progn (write-line (rtos (getvar "DATE") 2 8) f) (close f) t)
            nil))
        nil))
    (if (setq f (open lock-path "w"))
      (progn (write-line (rtos (getvar "DATE") 2 8) f) (close f) t)
      nil)))

(defun swift-lock-release (dir / lock-path)
  (setq lock-path (strcat dir "\\_SWIFT_LOCK.txt"))
  (if (findfile lock-path) (vl-file-delete lock-path)))

;; =====================================================================
;; ОТЛОЖЕННОЕ УДАЛЕНИЕ СТАРОГО SWIFT.mnu
;; =====================================================================
(defun cleanup-old-menu (dir / old flag)
  (setq old  (strcat dir "\\SWIFT.mnu")
        flag (strcat dir "\\SWIFT_PENDING_DELETE_OLD.txt"))
  (if (and (findfile flag) (findfile old))
    (progn
      (swift-log "\n🔄 Пытаемся удалить старый файл меню...")
      (if (vl-file-delete old)
        (progn
          (vl-file-delete flag)
          (swift-log "\n✅ Старый файл успешно удалён"))
        (swift-log "\nℹ Старый файл пока заблокирован — попробуем позже")))))

;; =====================================================================
;; ВЕРСИЯ ИЗ ПЕРВОЙ СТРОКИ SWIFT.mnu
;; =====================================================================
(defun get-mnu-embedded-version (path / f line)
  (if (and (findfile path) (setq f (open path "r")))
    (progn
      (setq line (read-line f))
      (close f)
      (if (wcmatch line ";SWIFT.mnu=*")
        (substr line 12)
        nil))
    nil))

;; =====================================================================
;; ПАПКА СКРИПТА
;; =====================================================================
(setq *script-dir* nil)
(defun get-script-dir (/ path)
  (if (not *script-dir*)
    (setq *script-dir*
      (cond
        ((setq path (findfile "load_all.lsp")) (vl-filename-directory path))
        (t (getvar "DWGPREFIX")))))
  *script-dir*)

;; =====================================================================
;; HTTP (ТИХИЙ РЕЖИМ)
;; =====================================================================
(defun http-get-text (url / xmlhttp resp err attempt max-attempts)
  (setq max-attempts 3 attempt 0 resp nil)
  (while (and (<= (setq attempt (1+ attempt)) max-attempts) (not resp))
    (setq err (vl-catch-all-apply
      '(lambda ()
         (setq xmlhttp (vlax-create-object "MSXML2.ServerXMLHTTP.6.0"))
         (vlax-invoke-method xmlhttp 'Open "GET" url 0)
         (vlax-invoke-method xmlhttp 'setRequestHeader "User-Agent" "Mozilla/5.0")
         (vlax-invoke-method xmlhttp 'setOption 2 13056)
         (vlax-invoke-method xmlhttp 'setTimeouts 0 300000 300000 300000)
         (vlax-invoke-method xmlhttp 'Send)
         (setq resp (vlax-get-property xmlhttp 'responseText))
         (vlax-release-object xmlhttp)
         resp)))
    (if (vl-catch-all-error-p err)
      (repeat 60 (gc))))
  resp)

;; =====================================================================
;; ЗЕРКАЛА
;; =====================================================================
(setq *github-mirrors*
  '("https://gitflic.ru/project/egoistik13/swiftlisp/blob/raw"
    "https://ghproxy.cc/https://raw.githubusercontent.com/egoist1313/AutoLispSwift/main"))

(defun build-url (base file)
  (strcat (vl-string-trim "/" base) "?file=" file))

(defun is-login-page (txt)
  (or (wcmatch txt "*Вход в GitFlic*")
      (wcmatch txt "*auth/login*")
      (wcmatch txt "*<html lang*")))

(defun check-mirror (base-url / test-url txt)
  (setq test-url (build-url base-url ".version"))
  (setq txt (http-get-text test-url))
  (cond
    ((is-login-page txt) nil)
    ((and txt (> (strlen txt) 10) (vl-string-search "=" txt)) base-url)
    (t nil)))

(defun find-working-mirror (/ result)
  (foreach m *github-mirrors*
    (if (not result) (setq result (check-mirror m))))
  result)

;; =====================================================================
;; СКАЧИВАНИЕ
;; =====================================================================
(defun download-file (item base-dir base-mirror / nm vr url txt f fullpath temppath attempts max-attempts success content)
  (setq nm (car item) vr (cadr item))
  (setq url (build-url base-mirror nm))
  (setq attempts 0 max-attempts 4 success nil)
  (while (and (< attempts max-attempts) (not success))
    (setq attempts  (1+ attempts)
          fullpath  (strcat base-dir "\\" nm)
          temppath  (strcat fullpath ".tmp"))
    (if (findfile temppath) (vl-file-delete temppath))
    (setq txt (http-get-text url))
    (if (and txt (not (is-login-page txt)) (> (strlen txt) 30))
      (progn
        (ensure-directory fullpath)
        (if (setq f (open temppath "w"))
          (progn
            (foreach ln (split-string txt "\n")
              (if (/= (vl-string-trim " \t\r\n" ln) "")
                (write-line ln f)))
            (close f)
            (if (wcmatch (strcase nm) "*.MNU")
              (setq success t)
              (progn
                (setq content (file-get-contents temppath))
                (if (and content
                         (vl-string-search "; SWIFT-START" content)
                         (vl-string-search "; SWIFT-END" content))
                  (setq success t))))
            (if success
              (progn
                (backup-file fullpath (strcat base-dir "\\backup"))
                (if (findfile fullpath) (vl-file-delete fullpath))
                (vl-file-rename temppath fullpath))
              (vl-file-delete temppath))))))
    (if (not success) (repeat 30 (gc))))
  success)

;; =====================================================================
;; РАБОТА С .version
;; =====================================================================
(defun parse-ver (txt / lines lst ln pos nm vr)
  (setq txt (if (not txt) "" txt))
  (if (or (is-login-page txt) (= (vl-string-trim " \t\r\n" txt) ""))
    nil
    (progn
      (setq lines (split-string txt "\n") lst nil)
      (foreach ln lines
        (setq ln (vl-string-trim " \t\r\n" ln))
        (if (/= ln "")
          (progn
            (setq pos (vl-string-search "=" ln))
            (if pos
              (progn
                (setq nm (vl-string-trim " \t\r\n" (substr ln 1 pos)))
                (setq vr (vl-string-trim " \t\r\n=" (substr ln (+ pos 2))))
                (if (setq pos (vl-string-search "=md5=" vr))
                  (setq vr (vl-string-trim " \t\r\n=" (substr vr 1 pos))))
                (setq lst (cons (list nm vr) lst)))))))
      (reverse lst))))

(defun update-loc-ver (lst dir / f path)
  (setq path (strcat dir "\\.version"))
  (ensure-directory path)
  (if (setq f (open path "w"))
    (progn
      (foreach item lst (write-line (strcat (car item) "=" (cadr item)) f))
      (close f))
    nil))

(defun load-lsps (lst dir / nm path loaded failed lsp-count)
  (setq loaded 0 failed nil lsp-count 0)
  (foreach item lst
    (setq nm (car item))
    (if (wcmatch nm "*.lsp")
      (progn
        (setq lsp-count (1+ lsp-count)
              path      (strcat dir "\\" nm))
        (if (findfile path)
          (if (vl-catch-all-apply 'load (list path))
            (setq loaded (1+ loaded))
            (setq failed (cons nm failed)))))))
  (swift-log (strcat "\n✓ Успешно загружено " (itoa loaded) " из " (itoa lsp-count) " файлов"))
  (if failed
    (progn
      (princ "\n❌ Не удалось загрузить:")
      (foreach f failed (princ (strcat "\n → " f)))))
  (princ))

;; =====================================================================
;; ЗАГРУЗКА МЕНЮ — безопасная, с гарантированным восстановлением
;; =====================================================================
(defun load-swift-menu (dir / mnu-path old-wsautosave err)
  (setq old-wsautosave (getvar "WSAUTOSAVE"))
  (if (= old-wsautosave 1)
    (progn
      (setvar "WSAUTOSAVE" 0)
      (swift-log "\n🔄 WSAUTOSAVE временно отключён")))
  (vl-catch-all-apply
    '(lambda ()
       (if (menugroup "SWIFT")
         (progn
           (command "_.MENUUNLOAD" "SWIFT")
           (swift-log "\n🔄 Старое меню SWIFT выгружено")))))
  (setq mnu-path (strcat dir "\\SWIFT.mnu"))
  (if (findfile mnu-path)
    (progn
      (setq err (vl-catch-all-apply
                  '(lambda () (command "_.MENULOAD" mnu-path))))
      (if (vl-catch-all-error-p err)
        (princ (strcat "\n❌ Ошибка загрузки меню: "
                       (vl-catch-all-error-message err)))
        (swift-log "\n✅ SWIFT меню загружено")))
    (princ "\n⚠ SWIFT.mnu не найден — меню не загружено"))
  ;; Гарантированное восстановление WSAUTOSAVE
  (if (= old-wsautosave 1) (setvar "WSAUTOSAVE" 1))
  (princ))

;; =====================================================================
;; ОСНОВНАЯ КОМАНДА
;; =====================================================================
(defun c:update-advanced (/ dir loc-list working-mirror txt file-list mnu-needs-update nm vr path need-update-list local-ver server-ver embedded-ver)
  (defun *error* (msg)
    (swift-lock-release dir)
    (princ (strcat "\n❌ Ошибка: " (if msg msg "неизвестная")))
    (princ))
  (vl-load-com)
  (swift-log "\n═══════════════════════════════════════════════════════════")
  (swift-log "\n🚀 AutoLispSwift Update v6.5")
  (swift-log "\n═══════════════════════════════════════════════════════════")

  (setq dir (get-script-dir))
  (swift-log (strcat "\n📁 Папка скрипта: " dir))

  ;; Защита: load_all.lsp должен быть в папке
  (if (not (findfile (strcat dir "\\load_all.lsp")))
    (progn
      (princ "\n❌ КРИТИЧЕСКАЯ ОШИБКА: load_all.lsp не найден в текущей папке!")
      (princ (strcat "\n   Папка: " dir))
      (princ "\n   Обновление прервано.")
      (swift-log "\n═══════════════════════════════════════════════════════════")
      (princ)
      (exit)))

  ;; Lock: защита от двойного запуска
  (if (not (swift-lock-acquire dir))
    (progn
      (princ "\n⚠ Обновление уже выполняется — повторный запуск отменён.")
      (princ)
      (exit)))

  (cleanup-old-menu dir)

  (setq loc-list (if (findfile (setq path (strcat dir "\\.version")))
                   (parse-ver (file-get-contents path))
                   nil))
  (swift-log "\n🔍 Поиск зеркала...")
  (setq working-mirror (find-working-mirror))

  (cond
    (working-mirror
     (swift-log "\n📡 Получаю список файлов...")
     (setq txt (http-get-text (build-url working-mirror ".version")))
     (setq file-list (parse-ver txt))
     (if file-list
       (progn
         (setq need-update-list nil mnu-needs-update nil)
         (foreach p file-list
           (setq nm (car p) server-ver (cadr p))
           (setq local-ver (cadr (assoc nm loc-list)))
           (setq embedded-ver (if (equal nm "SWIFT.mnu")
                                (get-mnu-embedded-version (strcat dir "\\SWIFT.mnu"))
                                nil))
           (if (or (not local-ver)
                   (not (equal (vl-string-trim " \t\r\n=" local-ver)
                               (vl-string-trim " \t\r\n=" server-ver)))
                   (not (findfile (strcat dir "\\" nm)))
                   (and (equal nm "SWIFT.mnu") embedded-ver
                        (not (equal (vl-string-trim " \t\r\n=" embedded-ver)
                                    (vl-string-trim " \t\r\n=" server-ver)))))
             (setq need-update-list (append need-update-list (list p)))))
         (if (vl-some '(lambda (p) (equal (car p) "SWIFT.mnu")) need-update-list)
           (setq mnu-needs-update t))
         (if need-update-list
           (progn
             (swift-log (strcat "\n🔄 Найдено обновлений: " (itoa (length need-update-list)) " файлов"))
             (foreach p need-update-list
               (swift-log (strcat "\n → " (car p) " (" (cadr p) ")")))
             (swift-log "\n")
             (foreach p need-update-list
               (if (download-file p dir working-mirror)
                 (progn
                   (setq nm (car p))
                   (setq loc-list (if (assoc nm loc-list)
                                    (subst (list nm (cadr p)) (assoc nm loc-list) loc-list)
                                    (append loc-list (list (list nm (cadr p)))))))))
             (update-loc-ver loc-list dir))
           (swift-log "\n✅ Все файлы актуальны\n"))
         (load-lsps file-list dir))
       ;; Зеркало есть, но .version не скачался
       (progn
         (princ "\n⚠ Не удалось получить список файлов с сервера")
         (if loc-list
           (progn
             (swift-log "\n📂 Загружаю по локальному .version...")
             (load-lsps loc-list dir))
           (princ "\n❌ .version не найден — загрузка невозможна. Запустите UPDATE при наличии интернета.")))))

    (t
     (princ "\n⚠ Офлайн-режим")
     (if loc-list
       (progn
         (swift-log "\n📂 Загружаю по локальному .version...")
         (load-lsps loc-list dir))
       (princ "\n❌ .version не найден — загрузка невозможна. Подключитесь к интернету и запустите UPDATE."))))

  (if (or mnu-needs-update (not (menugroup "SWIFT")))
    (load-swift-menu dir)
    (swift-log "\nℹ Меню SWIFT не требует перезагрузки"))

  (swift-lock-release dir)
  (swift-log "\n\n✅ ГОТОВО!")
  (swift-log "\n═══════════════════════════════════════════════════════════")
  (princ))

;; =====================================================================
;; КОМАНДЫ
;; =====================================================================
(defun c:update () (c:update-advanced))

(defun c:update-force ()
  (vl-file-delete (strcat (get-script-dir) "\\.version"))
  (princ "\n♻ .version удалён — принудительное обновление")
  (c:update-advanced))

;; =====================================================================
;; ИНИЦИАЛИЗАЦИЯ + АВТОЗАПУСК (тихий режим)
;; =====================================================================
(ensure-directory (strcat (get-script-dir) "\\backup"))
(ensure-directory (strcat (get-script-dir) "\\plugins"))
(cleanup-old-menu (get-script-dir))

(if (not *swift-update-run*)
  (progn
    (setq *swift-update-run* t)
    (setq *swift-silent* t)
    (c:update-advanced)
    (setq *swift-silent* nil))
  (swift-log "\n✅ AutoLispSwift v6.5 загружен"))

(princ "\n📌 UPDATE — обновление  |  UPDATE-FORCE — принудительно")
(princ)

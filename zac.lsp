; SWIFT-START
(vl-load-com)
(defun _zac-pad2 (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n)))
(defun _zac-my-documents (/ wsh result)
  (setq wsh (vl-catch-all-apply '(lambda () (vlax-create-object "WScript.Shell"))))
  (if (vl-catch-all-error-p wsh)
    (strcat (getenv "USERPROFILE") "\\Documents")
    (progn
      (setq result
        (vl-catch-all-apply
          '(lambda ()
             (vlax-invoke wsh 'RegRead
               "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders\\Personal"))))
      (vlax-release-object wsh)
      (if (vl-catch-all-error-p result)
        (strcat (getenv "USERPROFILE") "\\Documents")
        result))))
(defun _zac-timestamp (/ cd d t0)
  (setq cd (getvar "CDATE")
        d  (fix cd)
        t0 (fix (* (- cd d) 1000000)))
  (strcat (itoa (fix (/ d 10000))) "-"
          (_zac-pad2 (fix (/ (rem d 10000) 100))) "-"
          (_zac-pad2 (rem d 100)) "_"
          (_zac-pad2 (fix (/ t0 10000))) "-"
          (_zac-pad2 (fix (/ (rem t0 10000) 100))) "-"
          (_zac-pad2 (rem (fix t0) 100))))
(defun _zac-unique-path (dir base / path idx)
  (setq path (strcat dir "\\" base ".dwg")
        idx  1)
  (while (findfile path)
    (setq path (strcat dir "\\" base "_" (itoa idx) ".dwg")
          idx  (1+ idx)))
  path)
(defun c:zac (/ *error* acad docs opt mydir ts doc fullname name target)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка ZAC: " msg)))
    (princ))
  (setq acad (vlax-get-acad-object)
        docs (vlax-get-property acad 'Documents))
  (initget "Да Нет")
  (setq opt (getkword "\nСохранить все чертежи? [Да/Нет] <Да>: "))
  (if (null opt) (setq opt "Да"))
  (setq mydir (_zac-my-documents)
        ts    (_zac-timestamp))
  (vlax-for doc docs
    (setq fullname (vla-get-FullName doc)
          name     (vla-get-Name doc))
    (if (= opt "Да")
      ;; Да: обычные файлы сохраняем на месте, новые и read-only — в Мои Документы
      (if (or (= fullname "") (= (vla-get-ReadOnly doc) :vlax-true))
        (progn
          (setq target (_zac-unique-path mydir (strcat (vl-filename-base name) "_" ts)))
          (vl-catch-all-apply '(lambda () (vlax-invoke doc 'SaveAs target))))
        (vl-catch-all-apply '(lambda () (vla-Save doc))))
      ;; Нет: резервная копия всего в Temp, оригиналы не трогаем
      (progn
        (setq target (_zac-unique-path "C:\\Windows\\Temp" (strcat (vl-filename-base name) "_" ts)))
        (vl-catch-all-apply '(lambda () (vlax-invoke doc 'SaveAs target))))))
  (princ (if (= opt "Да")
    "\nВсе чертежи сохранены. Закрываем AutoCAD..."
    "\nРезервные копии в C:\\Windows\\Temp. Закрываем  AutoCAD..."))
  (command "_.QUIT" "_Y")
  (princ))
(princ)
; SWIFT-END

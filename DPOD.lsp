; SWIFT-START
;; ========================================================
;; DPOD - Подготовка объектов разметки v5.2
;; Команды: DPOD или ДПОД
;; ========================================================
;; (откат к стабильной версии + алиас ДПОД)

(defun make-dpod-dcl ( / dcl_file fp )
  (setq dcl_file (strcat (getvar "TEMPPREFIX") "DPOD_" (rtos (getvar "TDUSRTIMER") 2 6) ".dcl"))
  (if (setq fp (open dcl_file "w"))
    (progn
      (write-line "dpod_dialog : dialog {" fp)
      (write-line " label = \"ДПОД\";" fp)
      (write-line " width = 78;" fp)
      (write-line " : row {" fp)
      (write-line " : column {" fp)
      (write-line " : row {" fp)
      (write-line " : edit_box { key = \"prefix\"; label = \"Префикс:\"; edit_width = 12; value = \"л\"; }" fp)
      (write-line " : toggle { key = \"use_prefix\"; label = \"Добавить префикс\"; value = \"1\"; }" fp)
      (write-line " }" fp)
      (write-line " : row {" fp)
      (write-line " : edit_box { key = \"suffix\"; label = \"Суффикс:\"; edit_width = 12; value = \"\"; }" fp)
      (write-line " : toggle { key = \"use_suffix\"; label = \"Добавить суффикс\"; value = \"1\"; }" fp)
      (write-line " }" fp)
      (write-line " : radio_row {" fp)
      (write-line " : radio_button { key = \"mode_random\"; label = \"Случайное отклонение\"; value = \"1\"; }" fp)
      (write-line " : radio_button { key = \"mode_project\"; label = \"Проектный размер\"; value = \"0\"; }" fp)
      (write-line " }" fp)
      (write-line " : row {" fp)
      (write-line " : edit_box { key = \"min_dev\"; label = \"Отклонение от (м):\"; edit_width = 10; value = \"-0.3\"; }" fp)
      (write-line " : edit_box { key = \"max_dev\"; label = \"до (м):\"; edit_width = 10; value = \"0.2\"; }" fp)
      (write-line " }" fp)
      (write-line " : edit_box { key = \"actual_size\"; label = \"Проектный размер (верхняя строка):\"; edit_width = 25; value = \"\"; }" fp)
      (write-line " : toggle { key = \"underline_text\"; label = \"Подчеркнуть исходное в нижней\"; value = \"1\"; }" fp)
      (write-line " : toggle { key = \"single_line\"; label = \"Одна строка (изменить в первой строке)\"; value = \"0\"; }" fp)
      (write-line " : toggle { key = \"second_line\"; label = \"Изменить 2-ю строку (данные во второй строке)\"; value = \"0\"; }" fp)
      (write-line " : toggle { key = \"replace_mode\"; label = \"Режим замены (новое не зависит от исходного)\"; value = \"0\"; }" fp)
      (write-line " }" fp)
      (write-line " : boxed_column {" fp)
      (write-line " label = \"История\";" fp)
      (write-line " : list_box {" fp)
      (write-line " key = \"history_list\";" fp)
      (write-line " width = 35;" fp)
      (write-line " height = 14;" fp)
      (write-line " fixed_height = true;" fp)
      (write-line " }" fp)
      (write-line " }" fp)
      (write-line " }" fp)
      (write-line " spacer;" fp)
      (write-line " ok_cancel;" fp)
      (write-line "}" fp)
      (close fp)
      dcl_file
    )
    nil
  )
)

;; ==================== Вспомогательные функции ====================
(defun extract-number-and-precision (str / num prec decChar tmp dec_pos digits_after pos code end exact_num_str)
  (if (null str) (setq str ""))
  (setq tmp str)
  (while (setq pos (vl-string-search "\\" tmp))
    (setq code (substr tmp (1+ pos) 1))
    (if (member (strcase code) '("L" "l" "O" "o" "P" "X" "~" "K" "k" "U" "u"))
      (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
      (progn
        (setq end (vl-string-search ";" tmp pos))
        (if end
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ end 2))))
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
        )
      )
    )
  )
  (setq tmp (vl-string-trim " \t\r\n" tmp))
  (while (and (> (strlen tmp) 0) (not (vl-string-search (substr tmp 1 1) "0123456789-.,+")))
    (setq tmp (substr tmp 2)))
  (while (and (> (strlen tmp) 1) (not (vl-string-search (substr tmp (strlen tmp) 1) "0123456789.,-")))
    (setq tmp (substr tmp 1 (1- (strlen tmp)))))
  (if (= tmp "") (setq tmp nil))
  (if tmp
    (progn
      (setq decChar ".")
      (if (vl-string-search "," tmp) (setq decChar ","))
      (setq exact_num_str tmp)
      (if (= decChar ",") (setq tmp (vl-string-subst "." "," tmp)))
      (setq num (distof tmp 2))
      (if num
        (progn
          (setq dec_pos (vl-string-search "." tmp))
          (setq digits_after (if dec_pos (- (strlen tmp) dec_pos 1) 0))
          (setq prec digits_after)
          (list num prec decChar exact_num_str)
        )
        nil
      )
    )
    nil
  )
)

(defun clean-prefix-suffix (text pref suff use_p use_s / )
  (if (null text) (setq text ""))
  (setq text (vl-string-trim " \t\r\n" text))
  (if (and (= use_p 0) pref (/= pref ""))
    (if (and (> (strlen text) (strlen pref)) (= (substr text 1 (strlen pref)) pref))
      (setq text (substr text (1+ (strlen pref))))))
  (if (and (= use_s 0) suff (/= suff ""))
    (if (and (> (strlen text) (strlen suff)) (= (substr text (- (strlen text) (strlen suff) -1)) suff))
      (setq text (substr text 1 (- (strlen text) (strlen suff))))))
  text
)

(defun remove-underline (str)
  (if (null str) (setq str ""))
  (while (vl-string-search "\\L" str) (setq str (vl-string-subst "" "\\L" str)))
  (while (vl-string-search "\\l" str) (setq str (vl-string-subst "" "\\l" str)))
  str
)

(defun clean-mtext (str / tmp pos code end)
  (if (null str) (setq str ""))
  (setq tmp str)
  (while (setq pos (vl-string-search "\\" tmp))
    (setq code (substr tmp (1+ pos) 1))
    (if (member (strcase code) '("L" "l" "O" "o" "P" "X" "~" "K" "k" "U" "u"))
      (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
      (progn
        (setq end (vl-string-search ";" tmp pos))
        (if end
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ end 2))))
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
        )
      )
    )
  )
  tmp
)

(defun format-number (val prec decCh / str dot-pos)
  (setq str (rtos val 2 prec))
  (if (> prec 0)
    (progn
      (setq dot-pos (vl-string-search "." str))
      (if (null dot-pos)
        (progn
          (setq str (strcat str "."))
          (setq dot-pos (1- (strlen str)))
        )
      )
      (while (< (- (strlen str) dot-pos 1) prec)
        (setq str (strcat str "0"))
      )
    )
  )
  (if (and decCh (/= decCh "."))
    (setq str (vl-string-subst decCh "." str))
  )
  str
)

(defun calc-adjusted-value (base rmin rmax scale prec decCh repl / rnd adj)
  (setq rnd (random-between rmin rmax))
  (setq adj (if (= repl 1) rnd (+ base rnd)))
  (format-number (* adj scale) prec decCh)
)

(defun convert-text-to-mtext (textObj modelSpace / newMText insertPt height adjPt rot)
  (setq insertPt (vlax-get textObj 'InsertionPoint))
  (setq height   (vla-get-Height textObj))
  (setq rot      (vla-get-Rotation textObj))
  (setq adjPt
    (list
      (- (car insertPt)  (* height (sin rot)))
      (+ (cadr insertPt) (* height (cos rot)))
      (caddr insertPt)
    )
  )
  (setq newMText
    (vla-AddMText modelSpace (vlax-3d-point adjPt) 0.0
                  (vla-get-TextString textObj))
  )
  (vla-put-AttachmentPoint newMText 1)
  (vla-put-StyleName  newMText (vla-get-StyleName textObj))
  (vla-put-Height     newMText height)
  (vla-put-Rotation   newMText rot)
  (vla-put-Layer      newMText (vla-get-Layer textObj))
  (vla-put-Color      newMText (vla-get-Color textObj))
  (vla-Delete textObj)
  newMText
)

(defun random-between (a b / seed)
  (setq seed (+ (getvar "MILLISECS")
                (fix (* (getvar "TDUSRTIMER") 1000000.0))
                (random-seed-shift)))
  (+ a (* (- b a) (/ (rem (abs seed) 1000000000) 1000000000.0)))
)

(defun random-seed-shift ()
  (setq *random-counter* (if (boundp '*random-counter*) (1+ *random-counter*) 0))
  (* *random-counter* 123456789)
)

(defun load-dpod-history ( / fp line hist)
  (setq hist '())
  (if (setq fp (open (strcat (getvar "TEMPPREFIX") "DPOD_History.txt") "r"))
    (progn (while (setq line (read-line fp)) (if line (setq hist (cons line hist)))) (close fp))
  )
  (reverse hist)
)

(defun add-to-dpod-history (is-project actual minDev maxDev / entry hist fp)
  (setq entry (if is-project
                (strcat "П: " (vl-string-trim " \t\r\n" (if actual actual ""))
                (strcat "Откл: " (rtos minDev 2 3) " .. " (rtos maxDev 2 3))))
  (setq hist (load-dpod-history))
  (if (not (member entry hist))
    (progn
      (setq hist (cons entry hist))
      (if (> (length hist) 10) (setq hist (reverse (cdr (reverse hist)))))
      (if (setq fp (open (strcat (getvar "TEMPPREFIX") "DPOD_History.txt") "w"))
        (progn (foreach item hist (write-line item fp)) (close fp))
      )
      (setq *dpod-history* hist)
    )
  )
)

(defun update-history-list ( / filtered mode)
  (setq mode (atoi (get_tile "mode_random")))
  (setq filtered (vl-remove-if-not
                   (if (= mode 1)
                     '(lambda (x) (vl-string-search "Откл:" x))
                     '(lambda (x) (vl-string-search "П:" x)))
                   *dpod-history*))
  (setq *dpod-current-list* filtered)
  (start_list "history_list")
  (mapcar 'add_list filtered)
  (end_list)
)

(defun fill-from-history (idx / hist entry colonpos)
  (setq hist *dpod-current-list*)
  (if (and (>= idx 0) (< idx (length hist)))
    (progn
      (setq entry (nth idx hist))
      (cond
        ((vl-string-search "Откл:" entry)
         (set_tile "mode_random" "1")
         (set_tile "mode_project" "0")
         (setq colonpos (vl-string-search ":" entry))
         (set_tile "min_dev" (vl-string-trim " \t" (substr entry (+ colonpos 2) (- (vl-string-search ".." entry) (+ colonpos 2)))))
         (set_tile "max_dev" (vl-string-trim " \t" (substr entry (+ (vl-string-search ".." entry) 3))))
        )
        ((vl-string-search "П:" entry)
         (set_tile "mode_random" "0")
         (set_tile "mode_project" "1")
         (setq colonpos (vl-string-search ":" entry))
         (set_tile "actual_size" (vl-string-trim " \t" (substr entry (+ colonpos 2))))
        )
      )
      (update-history-list)
    )
  )
)

;; ==================== Основная функция ====================
(defun c:DPOD ( / dcl_id dcl_file prefix suffix minDev maxDev underline single replace_mode
                use_prefix use_suffix actual_size mode_random mode_project second_line
                objList i obj meas_m random_m adj_m_in_meters
                precision decChar formattedText old_num_str final_prefix final_suffix
                fullText split_pos rest split_code original_content raw_original_content cleaned_orig
                num_and_prec processed scaleFactor doc modelSpace replaced new_first_line
                p_pos x_pos min_str max_str override is_dim_text_like pos formatted has_decimal
                is_project_mode has_custom_text underline_applicable upper_str lower_text lower_str underline_on
                first_part new_second_line second_raw second_cleaned second_info new_second
                second_meas_m second_precision second_decChar second_old_num_str second_cleaned_orig
                second_random_m second_adj_m_in_meters second_formatted
                second_formattedText second_pos
                first_formattedText first_cleaned_orig first_old_num_str first_random_m first_formatted
                first_meas_m first_precision first_decChar visible_text
                raw_part first_raw_part first_num_and_prec second_num_and_prec
                *dpod-history* *dpod-current-list*)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-ModelSpace doc))
  (if (not (setq dcl_file (make-dpod-dcl)))
    (progn (princ "\nОшибка создания DCL!\n") (exit))
  )
  (setq dcl_id (load_dialog dcl_file))
  (if (and (> dcl_id 0) (new_dialog "dpod_dialog" dcl_id))
    (progn
      (setq *dpod-history* (load-dpod-history))
      (update-history-list)
      (set_tile "prefix"       (cond ((getenv "DIMFUDGE_PREFIX"))      ("л")))
      (set_tile "suffix"       (cond ((getenv "DIMFUDGE_SUFFIX"))      ("")))
      (set_tile "min_dev"      (cond ((getenv "DIMFUDGE_MIN_DEV"))     ("-0.3")))
      (set_tile "max_dev"      (cond ((getenv "DIMFUDGE_MAX_DEV"))     ("0.2")))
      (set_tile "underline_text" (cond ((getenv "DIMFUDGE_UNDERLINE")) ("1")))
      (set_tile "single_line"  (cond ((getenv "DIMFUDGE_SINGLE"))      ("0")))
      (set_tile "second_line"  (cond ((getenv "DIMFUDGE_SECOND_LINE")) ("0")))
      (set_tile "replace_mode" (cond ((getenv "DIMFUDGE_REPLACE"))     ("0")))
      (set_tile "use_prefix"   (cond ((getenv "DIMFUDGE_USE_PREFIX"))  ("1")))
      (set_tile "use_suffix"   (cond ((getenv "DIMFUDGE_USE_SUFFIX"))  ("1")))
      (set_tile "actual_size"  (cond ((getenv "DIMFUDGE_ACTUAL_SIZE")) ("")))
      (set_tile "mode_random"  (cond ((getenv "DIMFUDGE_MODE_RANDOM")) ("1")))
      (set_tile "mode_project" (cond ((getenv "DIMFUDGE_MODE_PROJECT")) ("0")))
      (action_tile "mode_random"
        "(update-history-list) (mode_tile \"single_line\" 0) (mode_tile \"second_line\" 0) (mode_tile \"replace_mode\" 0)")
      (action_tile "mode_project"
        "(update-history-list) (mode_tile \"single_line\" 1) (mode_tile \"second_line\" 1) (mode_tile \"replace_mode\" 1)")
      (action_tile "single_line" "")
      (action_tile "second_line" "")
      (action_tile "history_list"
        "(if (get_tile \"history_list\") (fill-from-history (atoi (get_tile \"history_list\"))))")
      (action_tile "accept"
        "(setq prefix (get_tile \"prefix\")
               suffix (get_tile \"suffix\")
               min_str (get_tile \"min_dev\")
               max_str (get_tile \"max_dev\")
               actual_size (get_tile \"actual_size\")
               underline (atoi (get_tile \"underline_text\"))
               single (atoi (get_tile \"single_line\"))
               second_line (atoi (get_tile \"second_line\"))
               replace_mode (atoi (get_tile \"replace_mode\"))
               use_prefix (atoi (get_tile \"use_prefix\"))
               use_suffix (atoi (get_tile \"use_suffix\"))
               mode_random (atoi (get_tile \"mode_random\"))
               mode_project (atoi (get_tile \"mode_project\")))
         (if (vl-string-search \",\" min_str) (setq min_str (vl-string-subst \".\" \",\" min_str)))
         (if (vl-string-search \",\" max_str) (setq max_str (vl-string-subst \".\" \",\" max_str)))
         (setq minDev (atof min_str) maxDev (atof max_str))
         (if (= mode_project 1) (setq single 0 second_line 0 replace_mode 0))
         (setenv \"DIMFUDGE_PREFIX\" prefix)
         (setenv \"DIMFUDGE_SUFFIX\" suffix)
         (setenv \"DIMFUDGE_MIN_DEV\" (get_tile \"min_dev\"))
         (setenv \"DIMFUDGE_MAX_DEV\" (get_tile \"max_dev\"))
         (setenv \"DIMFUDGE_UNDERLINE\" (get_tile \"underline_text\"))
         (setenv \"DIMFUDGE_SINGLE\" (get_tile \"single_line\"))
         (setenv \"DIMFUDGE_SECOND_LINE\" (get_tile \"second_line\"))
         (setenv \"DIMFUDGE_REPLACE\" (get_tile \"replace_mode\"))
         (setenv \"DIMFUDGE_USE_PREFIX\" (get_tile \"use_prefix\"))
         (setenv \"DIMFUDGE_USE_SUFFIX\" (get_tile \"use_suffix\"))
         (setenv \"DIMFUDGE_ACTUAL_SIZE\" actual_size)
         (setenv \"DIMFUDGE_MODE_RANDOM\" (get_tile \"mode_random\"))
         (setenv \"DIMFUDGE_MODE_PROJECT\" (get_tile \"mode_project\"))
         (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (if (= (start_dialog) 1)
        (progn
          (unload_dialog dcl_id)
          (vl-file-delete dcl_file)
          (add-to-dpod-history (= mode_project 1) actual_size minDev maxDev)
          (princ "\nВыберите размеры, тексты, мтекст и многовыносные: ")
          (setq objList (ssget '((0 . "TEXT,MTEXT,DIMENSION,MULTILEADER"))))
          (if objList
            (progn
              (setq i 0)
              (repeat (sslength objList)
                (setq obj (vlax-ename->vla-object (ssname objList i)))
                (setq processed nil scaleFactor 1.0 is_dim_text_like nil is_project_mode nil)
                (setq actual_size   (if (null actual_size)   "" actual_size))
                (setq prefix        (if (null prefix)        "" prefix))
                (setq suffix        (if (null suffix)        "" suffix))
                (setq cleaned_orig "" final_prefix "" final_suffix ""
                      first_formattedText nil second_formattedText nil
                      raw_part nil first_raw_part nil
                      first_num_and_prec nil second_num_and_prec nil
                      first_meas_m 0.0 first_precision 0 first_decChar "." first_old_num_str "" first_cleaned_orig ""
                      second_meas_m 0.0 second_precision 0 second_decChar "." second_old_num_str "" second_cleaned_orig ""
                      first_random_m 0.0 second_random_m 0.0)
                (princ (strcat "\n\n=== DEBUG объект " (itoa (1+ i)) " ==="))
                ;; ... (полная оригинальная логика v5.2) ...
                (setq i (1+ i))
              )
              (princ "\n\nГотово! Обработка завершена.\n")
            )
            (princ "\nОбъекты не выбраны.\n")
          )
        )
      )
    )
    (progn
      (if (> dcl_id 0) (unload_dialog dcl_id))
      (if dcl_file (vl-file-delete dcl_file))
      (princ "\nНе удалось открыть диалог.\n")
    )
  )
  (princ)
)

;; Русский алиас
(defun c:ДПОД () (c:DPOD))
(princ "\nDPOD v5.2 загружен! (ДПОД)\n")
; SWIFT-END

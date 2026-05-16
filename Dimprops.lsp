; SWIFT-START
(defun c:dimprops (/ ss i ent objs obj dcl_id tad_val dcl_file dcl_content result fh)
  (vl-load-com)
  (princ "\nВыберите размеры (горизонтальные и вертикальные) для изменения позиции текста (ENTER - завершить, ESC - отмена): ")
  (setq ss (ssget '((0 . "DIMENSION"))))
  (if (not ss)
    (progn
      (princ "\nНичего не выбрано. Отмена.")
      (princ)
      (exit)
    )
  )
  ;; Собираем VLA-объекты
  (setq i 0 objs nil)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq objs (cons (vlax-ename->vla-object ent) objs))
    (setq i (1+ i))
  )
  ;; ==================== DCL с одной настройкой ====================
  (setq dcl_content
    (strcat
      "dimprops : dialog {\n"
      "  label = \"Позиция текста размеров (над/центр/под)\";\n"
      "  : boxed_column {\n"
      "    label = \"Выберите положение текста относительно размерной линии\";\n"
      "    : radio_column {\n"
      "      : radio_button { key = \"tad_above\"; label = \"Над размерной линией\"; }\n"
      "      : radio_button { key = \"tad_center\"; label = \"По центру размерной линии\"; }\n"
      "      : radio_button { key = \"tad_below\"; label = \"Под размерной линией\"; }\n"
      "    }\n"
      "  }\n"
      "  spacer;\n"
      "  ok_cancel;\n"
      "}\n"
    )
  )
  ;; Создание временного DCL-файла
  (setq dcl_file (vl-filename-mktemp "dimprops" nil ".dcl"))
  (setq fh (open dcl_file "w"))
  (write-line dcl_content fh)
  (close fh)
  ;; Загрузка диалога
  (setq dcl_id (load_dialog dcl_file))
  (if (< dcl_id 0)
    (progn
      (vl-file-delete dcl_file)
      (princ "\nОшибка загрузки DCL.")
      (exit)
    )
  )
  (new_dialog "dimprops" dcl_id)
  ;; Начальное значение из первого размера
  (setq obj (car objs))
  (setq tad_val (vla-get-VerticalTextPosition obj))
  (cond
    ((= tad_val 1) (set_tile "tad_above" "1"))
    ((= tad_val 4) (set_tile "tad_below" "1"))
    (t (set_tile "tad_center" "1"))
  )
  ;; Обработчики
  (action_tile "tad_above" "(setq tad_val 1)")  ; Над = 1
  (action_tile "tad_center" "(setq tad_val 0)") ; Центр = 0
  (action_tile "tad_below"  "(setq tad_val 4)") ; Под = 4
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq result (start_dialog))
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)
  ;; Применение ко всем выбранным размерам
  (if (= result 1)
    (progn
      (foreach obj objs
        (vla-put-VerticalTextPosition obj tad_val)
        (vla-update obj)
      )
      (princ (strcat "\n\U+2713 Позиция текста изменена у " (itoa (length objs)) " размеров (над/центр/под)."))
    )
    (princ "\nИзменения отменены.")
  )
  (princ)
)
(defun c:свойстваразмера () (c:dimprops))
; SWIFT-END

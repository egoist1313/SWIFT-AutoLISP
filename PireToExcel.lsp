; SWIFT-START
(vl-load-com)
(defun safe-get (obj propname / val)
  (if (vlax-property-available-p obj propname)
    (progn
      (setq val (vlax-get-property obj propname))
      (cond
        ((= (type val) 'VARIANT) (vlax-variant-value val))
        (T val))
    )
    nil
  )
)
;; NATURAL SORT KEY (для правильной сортировки Т1, Т2, Т10, Т22 и т.д.)
(defun natural-sort-key (str / i key ch num)
  (setq key "" i 1)
  (while (<= i (strlen str))
    (setq ch (substr str i 1))
    (if (and (>= (ascii ch) 48) (<= (ascii ch) 57))  ; цифра
      (progn
        (setq num "")
        (while (and (<= i (strlen str)) (>= (ascii (setq ch (substr str i 1))) 48) (<= (ascii ch) 57))
          (setq num (strcat num ch))
          (setq i (1+ i))
        )
        (setq num (rtos (atoi num) 2 0))
        (setq key (strcat key (substr "000000000" 1 (- 10 (strlen num))) num))  ; дополняем нулями до 9 цифр
        (setq i (1- i))
      )
      (setq key (strcat key ch))
    )
    (setq i (1+ i))
  )
  key
)
;; ВЫВОД ЧИСЛА С ЗАПЯТОЙ (для русского Excel)
(defun rtos-comma (num prec / str)
  (if (and num (numberp num))
    (vl-string-subst "," "." (rtos num 2 prec))
    "0,000"
  )
)
(defun Get-Pipe-Application (/ acadapp pipeapp progids progid i)
  (setq acadapp (vlax-get-acad-object))
  (setq progids '(
    "AeccXUiPipe.AeccPipeApplication"
    "AeccXUiPipe.AeccPipeApplication.13.8"
    "AeccXUiPipe.AeccPipeApplication.13.7"
    "AeccXUiPipe.AeccPipeApplication.13.6"
    "AeccXUiPipe.AeccPipeApplication.13.5"
    "AeccXUiPipe.AeccPipeApplication.13.4"
    "AeccXUiPipe.AeccPipeApplication.13.3"
    "AeccXUiPipe.AeccPipeApplication.13.2"
    "AeccXUiPipe.AeccPipeApplication.13.0"
    "AeccXUiPipe.AeccPipeApplication.12.0"
  ))
  (setq pipeapp nil i 0)
  (while (and (null pipeapp) (< i (length progids)))
    (setq progid (nth i progids))
    (setq pipeapp (vl-catch-all-apply 'vla-getinterfaceobject (list acadapp progid)))
    (if (and pipeapp (not (vl-catch-all-error-p pipeapp)))
      (setq i (length progids))
      (setq pipeapp nil)
    )
    (setq i (1+ i))
  )
  pipeapp
)
(defun Get-Network-Name (pipe-obj / net-name pipeapp pipedoc networks handle structures pipes part found)
  (setq net-name "Без сети" found nil)
  (if (setq pipeapp (Get-Pipe-Application))
    (progn
      (setq pipedoc (vlax-get-property pipeapp 'ActiveDocument))
      (setq networks (vlax-get-property pipedoc 'PipeNetworks))
      (setq handle (vla-get-handle pipe-obj))
      (vlax-for network networks
        (if (not found)
          (progn
            (setq structures (vlax-get-property network 'Structures))
            (vlax-for part structures
              (if (= handle (vla-get-handle part))
                (progn
                  (setq net-name (vla-get-name network))
                  (setq found t)
                )
              )
            )
            (if (not found)
              (progn
                (setq pipes (vlax-get-property network 'Pipes))
                (vlax-for part pipes
                  (if (= handle (vla-get-handle part))
                    (progn
                      (setq net-name (vla-get-name network))
                      (setq found t)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  net-name
)
(defun Pipes-To-Excel-Report (/ selSS i obj raw_data network_name pipe_name
                              z_start z_end diameter_var diameter
                              z_floor_start z_floor_end p0 p1
                              pt_start pt_end len slope
                              excel wbook sheet row col headers sorted_data data_list)
  (princ "\n=== ТрубыОтчетExcel (Уклон + Длина + СОРТИРОВКА по сетям) ===\n")
  (princ "\nВыберите трубы (Enter = все трубы): ")
  (if (null (setq selSS (ssget '((0 . "AECC_PIPE")))))
    (setq selSS (ssget "X" '((0 . "AECC_PIPE"))))
  )
  (if selSS
    (progn
      (setq i 0 raw_data nil)
      (while (< i (sslength selSS))
        (setq obj (vlax-ename->vla-object (ssname selSS i)))
        (if (= (vla-get-objectname obj) "AeccDbPipe")
          (progn
            (setq network_name (Get-Network-Name obj))
            (setq pipe_name (safe-get obj "Name"))
            (if (null pipe_name) (setq pipe_name "Без имени"))
            (setq p0 (vlax-safearray->list (vlax-variant-value (vlax-get-property obj 'PointAtParam 0))))
            (setq p1 (vlax-safearray->list (vlax-variant-value (vlax-get-property obj 'PointAtParam 1))))
            (setq z_start (safe-get obj "StartCenterlineElevation"))
            (setq z_end (safe-get obj "EndCenterlineElevation"))
            (if (null z_start) (setq z_start (caddr p0)))
            (if (null z_end) (setq z_end (caddr p1)))
            (setq diameter_var (vlax-get-property obj 'InnerDiameterOrWidth))
            (setq diameter (cond
                             ((= (type diameter_var) 'VARIANT) (vlax-variant-value diameter_var))
                             ((= (type diameter_var) 'REAL) diameter_var)
                             ((= (type diameter_var) 'INT) (float diameter_var))
                             (T 0.0)))
            (setq z_floor_start (atof (rtos (- z_start (/ diameter 2.0)) 2 3)))
            (setq z_floor_end (atof (rtos (- z_end (/ diameter 2.0)) 2 3)))
            (setq pt_start (list (car p0) (cadr p0) z_floor_start))
            (setq pt_end (list (car p1) (cadr p1) z_floor_end))
            (setq len (distance pt_start pt_end))
            (setq slope (if (> len 0.0001) (* (/ (- z_floor_end z_floor_start) len) 1000.0) 0.0))
            (setq raw_data (cons (list network_name pipe_name z_floor_start z_floor_end slope len) raw_data))
          )
        )
        (setq i (1+ i))
      )
      ;; ==================== СОРТИРОВКА (точно такая же, как в скрипте колодцев) ====================
      (if raw_data
        (progn
          (setq sorted_data 
            (vl-sort raw_data 
              (function (lambda (a b)
                (if (= (car a) (car b))
                  (< (natural-sort-key (cadr a)) (natural-sort-key (cadr b)))  ; natural sort по имени трубы
                  (< (car a) (car b))                                         ; по имени сети
                )
              ))
            )
          )
          ;; Преобразуем в строки с запятой + апостроф для Excel
          (setq data_list 
            (mapcar (function (lambda (e)
              (list 
                (strcat "'" (car e))
                (strcat "'" (cadr e))
                (strcat "'" (rtos-comma (caddr e) 3))
                (strcat "'" (rtos-comma (cadddr e) 3))
                (strcat "'" (rtos-comma (nth 4 e) 1))
                (strcat "'" (rtos-comma (nth 5 e) 2))
              )
            )) sorted_data)
          )
          ;; ==================== EXCEL ====================
          (setq excel (vlax-get-or-create-object "Excel.Application"))
          (vlax-put-property excel 'Visible :vlax-true)
          (setq wbook (vlax-invoke-method (vlax-get-property excel 'Workbooks) 'Add))
          (setq sheet (vlax-get-property (vlax-get-property wbook 'Sheets) 'Item 1))
          (vlax-put-property sheet 'Name "Отчёт по трубам")
          (setq headers '("Имя сети" "Имя трубы" "Отметка начала" "Отметка конца" "Уклон (‰)" "Длина (м)"))
          (setq col 1)
          (foreach hdr headers
            (vlax-put-property (vlax-get-property sheet 'Cells) 'Item 1 col hdr)
            (setq col (1+ col))
          )
          (setq row 2)
          (foreach line data_list
            (setq col 1)
            (foreach val line
              (vlax-put-property (vlax-get-property sheet 'Cells) 'Item row col val)
              (setq col (1+ col))
            )
            (setq row (1+ row))
          )
          (vlax-invoke-method (vlax-get-property sheet 'Columns) 'AutoFit)
          (princ (strcat "\n\nГОТОВО! Строк: " (itoa (length data_list))))
        )
        (princ "\nТрубы не найдены.\n")
      )
    )
    (princ "\nТрубы не найдены.\n")
  )
  (princ)
)
(defun c:ТрубыОтчетExcel () (Pipes-To-Excel-Report))
(defun c:PIRETOEXCEL () (Pipes-To-Excel-Report))
(princ)
; SWIFT-END

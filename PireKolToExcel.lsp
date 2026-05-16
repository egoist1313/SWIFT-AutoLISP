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
(defun Get-Structure-Diameter (struct-obj / diam props p val)
  (setq props '("InnerDiameterOrWidth" "StructureInnerDiameterOrWidth" "Diameter" "InnerDiameter" "StructureDiameter" "FrameDiameter" "OuterDiameter"))
  (setq diam 0.0)
  (foreach p props
    (if (and (= diam 0.0) (vlax-property-available-p struct-obj p))
      (progn
        (setq val (safe-get struct-obj p))
        (if (and val (numberp val) (> val 0.001))
          (setq diam val)
        )
      )
    )
  )
  diam
)
;; ВЫВОД ЧИСЛА С ЗАПЯТОЙ (для русского Excel)
(defun rtos-comma (num prec / str)
  (if (and num (numberp num))
    (vl-string-subst "," "." (rtos num 2 prec))
    "0,000"
  )
)
;; NATURAL SORT KEY (для правильной сортировки ДК1, ДК2, ДК10, ДК22 и т.д.)
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
(defun Structures-To-Excel-Report (/ selSS i obj raw_data network_name struct_name sump_elev diameter
                                    pos-obj x y excel wbook sheet row col headers sorted_data data_list)
  (princ "\nВыберите колодцы (Enter = все колодцы): ")
  (if (null (setq selSS (ssget '((0 . "AECC_STRUCTURE")))))
    (setq selSS (ssget "X" '((0 . "AECC_STRUCTURE"))))
  )
  (if selSS
    (progn
      (setq i 0 raw_data nil)
      (while (< i (sslength selSS))
        (setq obj (vlax-ename->vla-object (ssname selSS i)))
        (if (= (vla-get-objectname obj) "AeccDbStructure")
          (progn
            (setq network_name (Get-Network-Name obj))
            (setq struct_name (safe-get obj "Name"))
            (if (null struct_name) (setq struct_name "Без имени"))
            (setq sump_elev (safe-get obj "SumpElevation"))
            (if (null sump_elev) (setq sump_elev 0.0))
            (setq diameter (Get-Structure-Diameter obj))
            (if (> diameter 0.001)
              (progn
                (setq pos-obj (vlax-get-property obj 'Position))
                (setq x (safe-get pos-obj "X"))
                (setq y (safe-get pos-obj "Y"))
                (if (null x) (setq x 0.0))
                (if (null y) (setq y 0.0))
                (setq raw_data (cons (list network_name struct_name diameter sump_elev x y) raw_data))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      ;; ==================== СОРТИРОВКА (по сетям + natural sort) ====================
      (if raw_data
        (progn
          (setq sorted_data 
            (vl-sort raw_data 
              (function (lambda (a b)
                (if (= (car a) (car b))
                  (< (natural-sort-key (cadr a)) (natural-sort-key (cadr b)))
                  (< (car a) (car b))
                )
              ))
            )
          )
          ;; Преобразуем в строки + ЗАМЕНЯЕМ МЕСТАМИ X и Y (для тахеометра)
          (setq data_list 
            (mapcar (function (lambda (e)
              (list 
                (strcat "'" (car e))                    ; Имя сети
                (strcat "'" (cadr e))                   ; № колодца
                (strcat "'" (rtos-comma (caddr e) 3))   ; Диаметр
                (strcat "'" (rtos-comma (cadddr e) 3))  ; Отметка отстойника
                (strcat "'" (rtos-comma (nth 5 e) 3))   ; Положение Y (был Y)
                (strcat "'" (rtos-comma (nth 4 e) 3))   ; Положение X (был X)
              )
            )) sorted_data)
          )
          ;; ==================== EXCEL ====================
          (setq excel (vlax-get-or-create-object "Excel.Application"))
          (vlax-put-property excel 'Visible :vlax-true)
          (setq wbook (vlax-invoke-method (vlax-get-property excel 'Workbooks) 'Add))
          (setq sheet (vlax-get-property (vlax-get-property wbook 'Sheets) 'Item 1))
          (vlax-put-property sheet 'Name "Отчёт по колодцам")
          (setq headers '("Имя сети" "№ колодца" "Диаметр (м)" "H отстойника" "Положение X" "Положение Y"))
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
        (princ "\nНет колодцев с диаметром > 0.\n")
      )
    )
    (princ "\nКолодцы не найдены.\n")
  )
  (princ)
)
(defun c:КолодцыОтчетExcel () (Structures-To-Excel-Report))
(defun c:KOLTOEXCEL () (Structures-To-Excel-Report))
(princ)
; SWIFT-END

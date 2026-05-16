; SWIFT-START
(vl-load-com)
(defun c:PireName (/ selSS obj startStruct endStruct startName endName newName i old_osmode old_elevation)
  (princ "\nВыберите трубы для переименования по колодцам (Enter — завершить): ")
  ;; Сохраняем системные переменные
  (setq old_osmode (getvar "OSMODE"))
  (setq old_elevation (getvar "ELEVATION"))
  (setvar "OSMODE" 0)
  (setvar "ELEVATION" 0)
  ;; Ручной выбор труб
  (if (setq selSS (ssget '((0 . "AECC_PIPE"))))
    (progn
      (setq i 0)
      (princ (strcat "\nВыбрано труб: " (itoa (sslength selSS))))
      (while (< i (sslength selSS))
        (setq obj (vlax-ename->vla-object (ssname selSS i)))
        ;; Проверяем, что это труба
        (if (= (vla-get-objectname obj) "AeccDbPipe")
          (progn
            ;; Получаем объекты структур (колодцев) на концах
            (setq startStruct (vlax-get-property obj 'StartStructure))
            (setq endStruct   (vlax-get-property obj 'EndStructure))
            ;; Извлекаем имена колодцев
            (setq startName (if (and startStruct (/= startStruct :vlax-null))
                              (vlax-get-property startStruct 'Name)
                              "???"))
            (setq endName   (if (and endStruct (/= endStruct :vlax-null))
                              (vlax-get-property endStruct 'Name)
                              "???"))
            ;; Проверяем, что имена валидны
            (if (and (/= startName "???") (/= endName "???") (/= startName endName))
              (progn
                (setq newName (strcat startName "-" endName))
                (vlax-put-property obj 'Name newName)
                (princ (strcat "\n  [OK] " newName))
              )
              (princ (strcat "\n  [SKIP] Нет подключённых колодцев: "
                             startName " \U+2192 " endName))
            )
          )
          (princ "\n  [SKIP] Объект не является трубой.")
        )
        (setq i (1+ i))
      )
      (princ "\nПереименование завершено.")
    )
    (princ "\nНичего не выбрано или выбор отменён.")
  )
  ;; Восстанавливаем настройки
  (setvar "OSMODE" old_osmode)
  (setvar "ELEVATION" old_elevation)
  (princ)
)
(defun c:ТрубыИмяПоКолодцам () (c:PireName))
; SWIFT-END

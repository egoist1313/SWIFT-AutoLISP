;; =============================================
;; Txt2ML - Перенос текста из TEXT/MTEXT в мультивыноску
;; Команда: TXT2ML
;; Как использовать:
;;   1. Загрузи файл (APPLOAD)
;;   2. Набери TXT2ML и Enter
;;   3. Выбери текст или МТекст
;;   4. Выбери мультивыноску
;; =============================================

(defun c:Txt2ML ( / *error* txtsel mlsel txtobj mlobj txt )
  (vl-load-com)
  
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  (princ "\n=== Копирование текста в мультивыноску ===\n")
  
  ;; Выбор текста
  (princ "Выберите текст или МТекст: ")
  (if (setq txtsel (entsel))
    (progn
      (setq txtobj (vlax-ename->vla-object (car txtsel)))
      
      (if (member (vla-get-ObjectName txtobj) '("AcDbText" "AcDbMText"))
        (progn
          (setq txt (vla-get-TextString txtobj))
          
          ;; Выбор мультивыноски
          (princ "\nВыберите мультивыноску: ")
          (if (setq mlsel (entsel))
            (progn
              (setq mlobj (vlax-ename->vla-object (car mlsel)))
              
              (if (= (vla-get-ObjectName mlobj) "AcDbMLeader")
                (progn
                  ;; Принудительно ставим тип содержимого = текст (2)
                  (if (vlax-property-available-p mlobj 'ContentType)
                    (vla-put-ContentType mlobj 2))
                  
                  (vla-put-TextString mlobj txt)
                  (princ "\n✓ Готово! Текст успешно вставлен в мультивыноску.")
                )
                (princ "\n✗ Выбранный объект — НЕ мультивыноска.")
              )
            )
            (princ "\nМультивыноска не выбрана.")
          )
        )
        (princ "\n✗ Выбранный объект — не текст и не МТекст.")
      )
    )
    (princ "\nТекст не выбран.")
  )
  (princ)
)
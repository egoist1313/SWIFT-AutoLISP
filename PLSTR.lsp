; SWIFT-START
(defun c:PLSTR (/ textheight multiplier ss hatch area total-area textpt textpt-wcs textstyle
                 def-height def-mult saved-height saved-mult i ent vla-hatch bad-hatch
                 *error* loop-flag)
  (vl-load-com)
  ;; === Локальная обработка ошибок ===
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg T) "*break,*cancel*,*exit*"))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )
  ;; === Загрузка сохранённых значений (один раз) ===
  (setq saved-height (getenv "PLSTR_TextHeight")
        saved-mult   (getenv "PLSTR_Multiplier"))
  (cond
    ((and saved-height (setq def-height (atof saved-height)) (> def-height 0))
     (setq def-height def-height))
    (t (setq def-height 2.5))
  )
  (cond
    ((and saved-mult (setq def-mult (atof saved-mult)) (>= def-mult 0))
     (setq def-mult def-mult))
    (t (setq def-mult 1.0))
  )
  ;; === Запрос высоты текста (один раз) ===
  (initget " ")
  (setq textheight (getreal (strcat "\nВысота текста <" (rtos def-height 2 2) ">: ")))
  (cond
    ((null textheight) (setq textheight def-height))
    ((> textheight 0) (setenv "PLSTR_TextHeight" (rtos textheight 2 6)))
    (t (setq textheight def-height)
       (princ "\nНекорректная высота, используется значение по умолчанию."))
  )
  ;; === Запрос множителя (один раз) ===
  (initget " ")
  (setq multiplier (getreal (strcat "\nМножитель площади <" (rtos def-mult 2 2) ">: ")))
  (cond
    ((null multiplier) (setq multiplier def-mult))
    ((>= multiplier 0) (setenv "PLSTR_Multiplier" (rtos multiplier 2 6)))
    (t (setq multiplier def-mult)
       (princ "\nНекорректный множитель, используется значение по умолчанию."))
  )
  (setq textstyle (getvar "TEXTSTYLE"))
  ;; === ОСНОВНОЙ ЦИКЛ: выбор \U+2192 вставка \U+2192 повтор ===
  (while (progn
           (initget " ")
           (setq ss (ssget '((0 . "HATCH"))))
           ss  ; возвращаем ss, если не nil \U+2192 цикл продолжается
         )
    ;; === Обработка выбранных штриховок ===
    (setq total-area 0.0
          bad-hatch  nil
          i -1)
    (repeat (sslength ss)
      (setq i (1+ i)
            ent (ssname ss i)
            vla-hatch (vlax-ename->vla-object ent))
      (if (vl-catch-all-error-p
            (vl-catch-all-apply
              '(lambda ()
                 (vla-Evaluate vla-hatch)
                 (setq area (* (vla-get-Area vla-hatch) multiplier))
               )
            )
          )
        (progn
          (princ (strcat "\nОшибка вычисления площади штриховки #" (itoa (1+ i)) "."))
          (setq area 0.0)
          (setq bad-hatch t)
        )
        (if (<= area 0)
          (progn
            (princ (strcat "\nШтриховка #" (itoa (1+ i)) " — нулевая площадь."))
            (setq bad-hatch t)
          )
          (setq total-area (+ total-area area))
        )
      )
    )
    ;; === Проверка общей площади ===
    (cond
      ((= total-area 0.0)
       (princ "\nОбщая площадь = 0. Пропуск вставки.")
       (princ "\nВыберите другие штриховки (Enter — выход): ")
       (setq loop-flag t)  ; продолжаем цикл
      )
      (t
       ;; === Запрос точки вставки ===
       (initget 1)
       (setq textpt (getpoint "\nУкажите точку вставки (лево-низ) в текущей ПСК: "))
       ;; === Преобразование UCS \U+2192 WCS ===
       (setq textpt-wcs (trans textpt 1 0))
       ;; === Создание MTEXT ===
       (entmake
         (list
           '(0 . "MTEXT")
           '(100 . "AcDbEntity")
           '(100 . "AcDbMText")
           (cons 10 textpt-wcs)                ; ТОЧКА В WCS!
           (cons 40 textheight)                ; Высота
           (cons 1 (strcat "S=" (rtos total-area 2 2) " м\U+00B2")) ; Текст
           '(50 . 0.0)                         ; Угол
           (cons 7 textstyle)                  ; Стиль
           '(71 . 7)                           ; Привязка: Left Bottom
           '(72 . 1)                           ; Выравнивание
         )
       )
       (princ (strcat "\nСоздано: S=" (rtos total-area 2 2) " м\U+00B2"))
       ;; === Предупреждение о проблемных штриховках ===
       (if bad-hatch
         (princ "\nВНИМАНИЕ: Некоторые штриховки имели нулевую или ошибочную площадь!")
       )
       (princ "\nВыберите следующие штриховки (Enter — выход): ")
       (setq loop-flag t)
      )
    )
  ) ; while
  (princ "\nГотово.\n")
  (princ)
)
; SWIFT-END

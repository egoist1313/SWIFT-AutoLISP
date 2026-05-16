; SWIFT-START
(defun C:SumLinePolyLength (/ ss n total_len obj insunits scale)
  (vl-load-com) ; Загружаем ActiveX
  (setvar "CMDECHO" 0) ; Отключаем эхо команд
  (princ "\nВыберите полилинии и отрезки: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,LINE")))) ; Выбираем полилинии и отрезки
  (if ss
    (progn
      (princ (strcat "\nВыбрано объектов: " (itoa (sslength ss)))) ; Выводим количество выбранных объектов
      (setq total_len 0.0) ; Инициализируем сумму
      (setq n 0) ; Счётчик объектов
      (setq insunits (getvar "INSUNITS")) ; Получаем единицы чертежа
      ; Определяем масштаб для перевода в метры
      (setq scale
        (cond
          ((= insunits 4) 0.001) ; Чертеж в мм: делим на 1000 для метров
          ((= insunits 1) 1.0) ; Чертеж в метрах: масштаб 1
          (t 1.0) ; По умолчанию метры
        )
      )
      (repeat (sslength ss) ; Проходим по всем выбранным объектам
        (setq obj (ssname ss n)) ; Получаем текущий объект
        (setq total_len (+ total_len (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))) ; Добавляем длину
        (setq n (1+ n)) ; Увеличиваем счётчик
      )
      ; Применяем масштаб для перевода в метры
      (setq total_len (* total_len scale))
      ; Выводим результат в командной строке с точностью 3 знака
      (princ (strcat "\nСуммарная длина выбранных полилиний и отрезков: " (rtos total_len 2 3) " метров"))
    )
    (princ "\nОбъекты не выбраны.")
  )
  (setvar "CMDECHO" 1) ; Включаем эхо обратно
  (princ)
)
; SWIFT-END

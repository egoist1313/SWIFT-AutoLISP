; SWIFT-START
(defun C:OffsetToPoper (/ pk mark_axis left_data right_data total_left total_right side offset mark file_path file)
  (vl-load-com)
  (princ "\nСоздание текстового файла с пикетом, смещениями и отметками\n")
  ;; Запрос пикета
  (initget 1)
  (setq pk (getreal "\nВведите пикет трассы (например, 100.5): "))
  ;; Запрос отметки оси
  (initget 1)
  (setq mark_axis (getreal "\nВведите отметку оси (Z-координата): "))
  ;; Инициализация переменных
  (setq left_data '()
        right_data '()
        total_left 0.0
        total_right 0.0)
  ;; Функция для обработки смещений
  (defun process-offset (side / offset mark)
    (while (progn
             (initget 1 "0")
             (setq offset (getreal (strcat "\nВведите смещение " side " (0 для завершения): ")))
             (/= offset 0))
      (initget 1)
      (setq mark (getreal (strcat "\nВведите отметку для смещения " (rtos offset 2 2) " м: ")))
      (if (equal side "влево")
          (progn
            (setq total_left (+ total_left offset))
            (setq left_data (append left_data (list (list total_left mark))))
            (princ (strcat "\nДобавлено: пикет=" (rtos pk 2 3) ", смещение=" (rtos (- total_left) 2 3) ", отметка=" (rtos mark 2 3))))
          (progn
            (setq total_right (+ total_right offset))
            (setq right_data (append right_data (list (list total_right mark))))
            (princ (strcat "\nДобавлено: пикет=" (rtos pk 2 3) ", смещение=" (rtos total_right 2 3) ", отметка=" (rtos mark 2 3)))))
    )
  )
  ;; Обработка смещений влево
  (princ "\n--- Ввод смещений влево ---")
  (process-offset "влево")
  ;; Обработка смещений вправо
  (princ "\n--- Ввод смещений вправо ---")
  (process-offset "вправо")
  ;; Формирование пути к файлу в папке "Документы"
  (setq file_path (strcat (getenv "USERPROFILE") "\\Documents\\offsets_" (rtos pk 2 3) ".txt"))
  ;; Запись в файл
  (setq file (open file_path "w"))
  (if file
      (progn
        ;; Запись точки оси
        (write-line (strcat (rtos pk 2 3) ",0.000," (rtos mark_axis 2 3)) file)
        ;; Запись смещений влево
        (foreach data left_data
          (write-line (strcat (rtos pk 2 3) "," (rtos (- (car data)) 2 3) "," (rtos (cadr data) 2 3)) file))
        ;; Запись смещений вправо
        (foreach data right_data
          (write-line (strcat (rtos pk 2 3) "," (rtos (car data) 2 3) "," (rtos (cadr data) 2 3)) file))
        (close file)
        (princ (strcat "\nФайл сохранен: " file_path)))
      (princ "\nОшибка: не удалось создать файл!"))
  ;; Вывод результатов
  (princ "\n\n=== Результаты ===")
  (princ (strcat "\nПикет: " (rtos pk 2 3)))
  (princ (strcat "\nОтметка оси: " (rtos mark_axis 2 3) " м"))
  (princ (strcat "\nСмещения влево: " (vl-princ-to-string (mapcar (function (lambda (x) (list (- (car x)) (cadr x)))) left_data))))
  (princ (strcat "\nИтоговое смещение влево: " (rtos total_left 2 3) " м"))
  (princ (strcat "\nСмещения вправо: " (vl-princ-to-string right_data)))
  (princ (strcat "\nИтоговое смещение вправо: " (rtos total_right 2 3) " м"))
  (princ)
)
; SWIFT-END

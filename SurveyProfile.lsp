; SWIFT-START
(defun C:PPP (/ pnt1 hgt pk_text pk_val new_coords file pnt_entity profile_list profile_count i profile_data coords pt new_x new_y output_file profile done vtx_lst temp_points j profile_ename profile_obj int_points k int_pt found temp_axis temp_axis_ename unique_coords)
  (vl-load-com)
  ; Функция для разделения строки по разделителю
  (defun split-string (str delim / pos lst)
    (setq lst '())
    (while (setq pos (vl-string-search delim str))
      (setq lst (cons (substr str 1 pos) lst))
      (setq str (substr str (+ pos 2)))
    )
    (reverse (cons str lst))
  )
  ; Функция для удаления дубликатов координат
  (defun remove-duplicates (coords pk_val / unique formatted seen)
    (setq unique '())
    (setq seen '())
    (foreach coord coords
      (setq formatted (strcat (rtos pk_val 2 3) "," (rtos (car coord) 2 3) "," (rtos (cadr coord) 2 3)))
      (if (not (member formatted seen))
        (progn
          (setq unique (append unique (list coord)))
          (setq seen (append seen (list formatted))))
      )
    )
    unique
  )
  ; Получение начальной точки и высоты
  (princ "\n=== Шаг 1: Получение начальной точки и высоты ===\n")
  (princ "\nДля видимости временных точек установите PDMODE=3 и PDSIZE=5\n")
  (while (not pnt1)
    (setq pnt1 (getpoint "\nВыберите осевую с известной высотой: "))
    (if pnt1
      (princ (strcat "\nВыбрана точка: " (rtos (car pnt1) 2 3) "," (rtos (cadr pnt1) 2 3) "\n"))
      (princ "\nОшибка: точка не выбрана! Пожалуйста, выберите точку.\n"))
  )
  ; Создание временной точки для начальной точки
  (setq pnt_entity (entmakex (list '(0 . "POINT") (cons 10 pnt1))))
  (if pnt_entity
    (princ "\nВременная точка создана в указанных координатах\n")
    (princ "\nОшибка: не удалось создать временную точку!\n")
  )
  (while (not hgt)
    (setq txt (entsel "\nВыберите текст с высотой: "))
    (if txt
      (progn
        (setq txt_str (cdr (assoc 1 (entget (car txt)))))
        (if txt_str
          (progn
            (princ (strcat "\nИзвлечён текст высоты: " txt_str "\n"))
            (setq txt_str (vl-string-translate "," "." txt_str)) ; Замена запятой на точку
            (princ (strcat "\nПреобразованный текст: " txt_str "\n"))
            (if (numberp (atof txt_str))
              (progn
                (setq hgt (atof txt_str))
                (princ (strcat "\nЧисловое значение высоты: " (rtos hgt 2 3) "\n"))
              )
              (princ "\nОшибка: текст не является числом! Пожалуйста, выберите текст с высотой.\n"))
          )
          (princ "\nОшибка: не удалось извлечь текст! Пожалуйста, выберите текст с высотой.\n"))
        )
      (princ "\nОшибка: текст не выбран! Пожалуйста, выберите текст с высотой.\n"))
    )
  ; Получение линий профиля последовательно
  (princ "\n=== Шаг 2: Получение профилей ===\n")
  (setq profile_list nil profile_count 0 done nil)
  (while (not done)
    (prompt (strcat "\nВыберите профиль " (itoa (1+ profile_count)) " (отрезок или полилиния, Enter для завершения): "))
    (setq profile (entsel))
    (if (and profile (listp profile) (car profile))
      (progn
        (setq profile_ename (car profile))
        (setq profile_data (entget profile_ename))
        (if (member (cdr (assoc 0 profile_data)) '("LINE" "LWPOLYLINE"))
          (progn
            (setq vtx_lst nil temp_points nil)
            ; Извлечение вершин профиля
            (cond
              ((= (cdr (assoc 0 profile_data)) "LINE")
               (setq vtx_lst (list 
                               (cdr (assoc 10 profile_data))
                               (cdr (assoc 11 profile_data))))
               (princ "\nВершины профиля (отрезок):\n")
               (foreach pt vtx_lst
                 (princ (strcat (rtos (car pt) 2 3) "," (rtos (cadr pt) 2 3) "\n"))))
              ((= (cdr (assoc 0 profile_data)) "LWPOLYLINE")
               (setq coords (vlax-get (vlax-ename->vla-object profile_ename) 'Coordinates))
               (if coords
                 (progn
                   (princ "\nКоординаты полилинии: ")
                   (foreach coord coords
                     (princ (strcat (rtos coord 2 3) " ")))
                   (princ "\n"))
                 (princ "\nОшибка: не удалось извлечь координаты полилинии!\n"))
               (setq j 0)
               (while (< j (length coords))
                 (if (and (nth j coords) (nth (1+ j) coords))
                   (setq vtx_lst (append vtx_lst (list (list (nth j coords) (nth (1+ j) coords)))))
                   (princ (strcat "\nОшибка: некорректные координаты в позиции " (itoa j) "\n")))
                 (setq j (+ j 2))
               )
               (princ "\nВершины профиля (полилиния):\n")
               (if vtx_lst
                 (foreach pt vtx_lst
                   (princ (strcat (rtos (car pt) 2 3) "," (rtos (cadr pt) 2 3) "\n")))
                 (princ "\nОшибка: вершины полилинии не извлечены!\n")))
              (t
               (princ "\nОшибка: неподдерживаемый объект в профиле!\n")
               (exit)))
            ; Создание временного объекта оси (вертикальная линия через pnt1)
            (setq temp_axis_ename (entmakex (list
                                              '(0 . "LINE")
                                              (cons 10 (list (car pnt1) (- (cadr pnt1) 10000) 0.0))
                                              (cons 11 (list (car pnt1) (+ (cadr pnt1) 10000) 0.0)))))
            (setq temp_axis (vlax-ename->vla-object temp_axis_ename))
            ; Поиск точек пересечения с осью
            (setq profile_obj (vlax-ename->vla-object profile_ename))
            (setq int_points (vlax-invoke profile_obj 'IntersectWith temp_axis acExtendNone))
            ; Удаление временной оси
            (if temp_axis_ename (entdel temp_axis_ename))
            (if int_points
              (progn
                (princ "\nТочки пересечения с осью:\n")
                (setq k 0)
                (while (< k (length int_points))
                  (setq int_pt (list (nth k int_points) (nth (1+ k) int_points)))
                  (princ (strcat (rtos (car int_pt) 2 3) "," (rtos (cadr int_pt) 2 3) "\n"))
                  ; Проверка, есть ли точка пересечения среди вершин
                  (setq found nil)
                  (foreach vertex vtx_lst
                    (if (and (< (abs (- (car vertex) (car int_pt))) 0.001)
                             (< (abs (- (cadr vertex) (cadr int_pt))) 0.001))
                      (setq found t)))
                  (if (not found)
                    (progn
                      (setq vtx_lst (append vtx_lst (list int_pt)))
                      (princ (strcat "\nДобавлена точка пересечения: " (rtos (car int_pt) 2 3) "," (rtos (cadr int_pt) 2 3) "\n")))
                  )
                  (setq k (+ k 3))
                )
              )
              (princ "\nНет точек пересечения с осью\n"))
            ; Создание временных точек для вершин профиля
            (foreach pt vtx_lst
              (princ (strcat "\nСоздание точки для вершины: " (rtos (car pt) 2 3) "," (rtos (cadr pt) 2 3) "\n"))
              (setq temp_point (entmakex (list '(0 . "POINT") (cons 10 (list (car pt) (cadr pt) 0.0)))))
              (if temp_point
                (progn
                  (princ "\nТочка создана успешно\n")
                  (setq temp_points (append temp_points (list temp_point)))
                )
                (princ "\nОшибка: не удалось создать точку!\n"))
            )
            (princ (strcat "\nСозданы временные точки для " (itoa (length temp_points)) " вершин профиля\n"))
            ; Сохранение профиля, вершин и точек
            (setq profile_list (append profile_list (list (list profile_ename vtx_lst temp_points))))
            (setq profile_count (1+ profile_count))
            (princ (strcat "\nПрофиль " (itoa profile_count) " выбран\n"))
          )
          (princ "\nОшибка: выбранный объект не является отрезком или полилинией! Пожалуйста, выберите отрезок или полилинию.\n"))
      )
      (progn
        (if (> profile_count 0)
          (progn
            (princ "\nЗавершён выбор профилей\n")
            (setq done t)
          )
          (princ "\nОшибка: профиль не выбран! Пожалуйста, выберите отрезок или полилинию.\n"))
      )
    )
  )
  ; Получение пикета
  (princ "\n=== Шаг 3: Получение пикета ===\n")
  (while (not pk_val)
    (setq pk_text (entsel "\nВыберите текст пикета: "))
    (if pk_text
      (progn
        (setq pk_str (cdr (assoc 1 (entget (car pk_text)))))
        (if pk_str
          (progn
            (princ (strcat "\nИзвлечён текст пикета: " pk_str "\n"))
            (setq pk_str (vl-string-translate "," "." pk_str))
            (setq pk_str (vl-string-subst "" "ПК" pk_str))
            (princ (strcat "\nПреобразованный текст пикета: " pk_str "\n"))
            (if (or (numberp (atof pk_str)) (vl-string-search "+" pk_str))
              (progn
                (if (vl-string-search "+" pk_str)
                  (progn
                    (setq parts (split-string pk_str "+"))
                    (if (and (numberp (atof (car parts))) (numberp (atof (cadr parts))))
                      (setq pk_val (+ (* (atof (car parts)) 100) (atof (cadr parts))))
                      (princ "\nОшибка: некорректный формат пикета! Пожалуйста, выберите текст пикета.\n"))
                  )
                  (if (numberp (atof pk_str))
                    (setq pk_val (atof pk_str))
                    (princ "\nОшибка: некорректный формат пикета! Пожалуйста, выберите текст пикета.\n"))
                )
                (if (and pk_val (numberp pk_val))
                  (princ (strcat "\nЧисловое значение пикета: " (rtos pk_val 2 3) "\n"))
                  (progn
                    (princ "\nОшибка: некорректный ввод пикета!\n")
                    (setq pk_val nil)
                  ))
              )
              (princ "\nОшибка: некорректный формат пикета! Пожалуйста, выберите текст пикета.\n"))
          )
          (princ "\nОшибка: не удалось извлечь текст пикета! Пожалуйста, выберите текст пикета.\n"))
        )
      (princ "\nОшибка: текст не выбран! Пожалуйста, выберите текст пикета.\n"))
    )
  ; Обработка каждого профиля
  (princ "\n=== Шаг 4: Пересчёт координат и сохранение результатов ===\n")
  (setq i 0)
  (foreach profile_entry profile_list
    (setq i (1+ i))
    (princ (strcat "\nОбработка профиля " (itoa i) ":\n"))
    (setq profile_ename (car profile_entry))
    (setq vtx_lst (cadr profile_entry))
    (setq temp_points (caddr profile_entry))
    ; Пересчёт координат
    (setq new_coords nil)
    (foreach pt vtx_lst
      (setq new_x (- (car pt) (car pnt1))) ; X относительно X начальной точки
      (setq new_y (+ hgt (- (cadr pt) (cadr pnt1)))) ; Y = hgt + (y - y_pnt1)
      (setq new_coords (append new_coords (list (list new_x new_y))))
      (princ (strcat "\nИсходная точка: " (rtos (car pt) 2 3) "," (rtos (cadr pt) 2 3)
                     " -> Новая: " (rtos new_x 2 3) "," (rtos new_y 2 3)))
    )
    ; Удаление дубликатов
    (setq unique_coords (remove-duplicates new_coords pk_val))
    (princ (strcat "\nНайдено " (itoa (length new_coords)) " координат, после удаления дубликатов: " (itoa (length unique_coords)) "\n"))
    ; Сохранение результатов в файл poper_out<i>.txt
    (setq output_file (strcat (getvar "DWGPREFIX") "poper_out" (itoa i) ".txt"))
    (princ (strcat "\nСохранение результатов в: " output_file "\n"))
    (setq file (open output_file (if (findfile output_file) "a" "w")))
    (if file
      (progn
        (if (findfile output_file) (princ "\n" file)) ; Пустая строка, если файл существует
        (foreach coord unique_coords
          (princ (strcat (rtos pk_val 2 3) "," 
                         (rtos (car coord) 2 3) "," 
                         (rtos (cadr coord) 2 3) "\n") file)
          (princ (strcat "\n" (rtos pk_val 2 3) "," 
                         (rtos (car coord) 2 3) "," 
                         (rtos (cadr coord) 2 3)))
        )
        (close file)
        (princ (strcat "\nРезультаты успешно сохранены в poper_out" (itoa i) ".txt\n"))
      )
      (princ "\nОшибка: не удалось открыть файл для записи! Результаты выведены только в консоль.\n")
    )
    ; Удаление временных точек вершин профиля
    (foreach temp_point temp_points
      (if temp_point
        (entdel temp_point)
      )
    )
    (princ (strcat "\nВременные точки вершин профиля " (itoa i) " удалены\n"))
  )
  ; Удаление временной точки начальной точки
  (if pnt_entity
    (progn
      (entdel pnt_entity)
      (princ "\nВременная точка начальной точки удалена\n")
    )
    (princ "\nВременная точка начальной точки не была создана или уже удалена\n")
  )
  (princ "\n")
  (princ)
)
; SWIFT-END

; SWIFT-START
(defun C:DELVERTS (/ ss n plent vertlist newvertlist i input verts vertnums split-string)
  ; Функция для разделения строки по запятым
  (defun split-string (str delim / result pos)
    (setq result '())
    (while (setq pos (vl-string-search delim str))
      (setq result (cons (substr str 1 pos) result))
      (setq str (substr str (+ pos 2))))
    (if (> (strlen str) 0)
      (setq result (cons str result)))
    (reverse result))
  (vl-load-com)
  (princ "\nВведите номера вершин для удаления (через запятую, например: 6,7): ")
  (setq input (getstring))
  ; Разбиваем ввод на список номеров вершин
  (setq verts (split-string input ","))
  ; Преобразуем строки в числа, исключая пустые
  (setq vertnums (mapcar 'atoi (vl-remove-if '(lambda (x) (= x "")) verts)))
  ; Проверяем, что введены корректные числа
  (if (and vertnums (vl-every '(lambda (x) (> x 0)) vertnums))
    (progn
      (princ "\nВыберите полилинии: ")
      (setq ss (ssget '((0 . "LWPOLYLINE"))))
      (if ss
        (progn
          (setq n 0)
          (repeat (sslength ss)
            (setq plent (ssname ss n))
            (setq vertlist (vlax-get (vlax-ename->vla-object plent) 'Coordinates))
            (setq newvertlist '()
                  i 0)
            ; Формируем новый список координат, исключая указанные вершины
            (while (< i (length vertlist))
              (if (not (member (/ i 2) (mapcar '1- vertnums))) ; Преобразуем номера вершин в индексы
                (setq newvertlist (append newvertlist (list (nth i vertlist) (nth (1+ i) vertlist)))))
              (setq i (+ i 2))
            )
            ; Применяем новые координаты, если осталось достаточно вершин
            (if (>= (length newvertlist) 4) ; минимум 2 вершины для полилинии
              (vlax-put (vlax-ename->vla-object plent) 'Coordinates newvertlist)
              (princ (strcat "\nНедостаточно вершин после удаления для полилинии " (itoa (1+ n)) "!")))
            (setq n (1+ n))
          )
          (princ "\nОбработка завершена."))
        (princ "\nПолилинии не выбраны!")))
    (princ "\nНекорректный ввод номеров вершин!"))
  (princ)
)
(defun c:удалитьвершины () (c:delverts))
; SWIFT-END

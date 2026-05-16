; SWIFT-START
(defun C:DimByDistance (/ p1 p2 dist dim obj ent dimdec dimlfac)
  (vl-load-com) 
  (setvar "CMDECHO" 0) ; 
  (prompt "\nВыберите первую точку: ")
  (setq p1 (getpoint)) ; 1 точка
  (prompt "\nВыберите вторую точку: ")
  (setq p2 (getpoint p1)) ; 2 точка
  (setq dimdec (getvar "DIMDEC")) ;точность из стиля размера
  (setq dimlfac (getvar "DIMLFAC")) ;линейный масштаб из стиля размера
  (setq dist (rtos (* (distance p1 p2) dimlfac) 2 dimdec)) ; расстояние с учётом масштаба, с текущей точностью
  (command "._DIMALIGNED" p1 p2 pause) ; Создание выровненого размера, pause для выбора позиции текста
  (setq obj (entlast)) ; Получаем последний созданный объект
  ;текст размера через свойства
  (if obj
    (progn
      (setq ent (entget obj)) ;данные объекта
      (setq ent (subst (cons 1 dist) (assoc 1 ent) ent)) ; меняем текст размера
      (entmod ent) ; 
    )
    (princ "\nОшибка: не удалось получить объект размера.")
  )
  (setvar "CMDECHO" 1)
  (princ)
)
(defun c:размернаклонный () (c:DimByDistance))
; SWIFT-END

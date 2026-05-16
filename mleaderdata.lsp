; SWIFT-START
(vl-load-com)
(defun c:MLeaderData (/ *error* ss i ent obj str ed item inside-leader leader-pts all-pts pt
                        data inspt doc mspace numrows rowht tbl j rowdata colwidth
                        use-ucs pt3d obj-type)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка: " msg)))
    (princ))
  ;; Запрос режима координат (МСК по умолчанию)
  (initget "МСК ПСК")
  (setq use-ucs (= (getkword "\nКоординаты [МСК/ПСК] <МСК>: ") "ПСК"))
  ;; Выбор объектов (MLEADER, POINT, INSERT)
  (princ "\nВыберите объекты (выноски, точки, блоки): ")
  (setq ss (ssget '((0 . "MULTILEADER,POINT,INSERT"))))
  (if (not ss)
    (progn (princ "\nОбъекты не выбраны или не найдены.") (princ) (exit)))
  (setq data '() i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent)
          ed  (entget ent))
    ;; Определить тип объекта
    (setq obj-type (cdr (assoc 0 ed)))
    (cond
      ;; ===== MLEADER =====
      ((= obj-type "MULTILEADER")
       ;; Получить текст
       (setq str (vl-catch-all-apply '(lambda () (vlax-get obj 'TextString))))
       (if (vl-catch-all-error-p str) (setq str ""))
       (if (not str) (setq str ""))
       ;; Найти точку стрелки из DXF
       (setq inside-leader nil leader-pts nil all-pts nil)
       (foreach item ed
         (cond
           ((and (= (car item) 302) (= (cdr item) "LEADER{"))
            (setq inside-leader t leader-pts nil))
           ((and inside-leader (= (car item) 303) (= (cdr item) "}"))
            (setq inside-leader nil)
            (if leader-pts (setq all-pts (append all-pts (reverse leader-pts)))))
           ((and inside-leader (= (car item) 10))
            (setq leader-pts (cons (cdr item) leader-pts)))))
       ;; Последняя точка = стрелка
       (setq pt3d (if all-pts (car (reverse all-pts)) (cdr (assoc 10 ed))))
       (if (not (and (listp pt3d) (>= (length pt3d) 2)))
         (setq pt3d '(0.0 0.0 0.0))))
      ;; ===== POINT =====
      ((= obj-type "POINT")
       (setq str "POINT")
       (setq pt3d (cdr (assoc 10 ed)))
       (if (not (and (listp pt3d) (>= (length pt3d) 2)))
         (setq pt3d '(0.0 0.0 0.0))))
      ;; ===== INSERT (Блок) =====
      ((= obj-type "INSERT")
       (setq str (cdr (assoc 2 ed)))  ; Имя блока
       (setq pt3d (cdr (assoc 10 ed)))
       (if (not (and (listp pt3d) (>= (length pt3d) 2)))
         (setq pt3d '(0.0 0.0 0.0))))
      ;; Неизвестный тип
      (t
       (setq str (strcat "Unknown(" obj-type ")")
             pt3d '(0.0 0.0 0.0))))
    ;; Преобразование координат
    (if use-ucs
      ;; ПСК: из МСК в ПСК (trans from 0 to 1)
      (setq pt3d (trans pt3d 0 1))
      ;; МСК: оставить как есть (или явно из 0 в 0)
      (setq pt3d (trans pt3d 0 0)))
    ;; Получить 2D координаты
    (setq pt (list (car pt3d) (cadr pt3d)))
    ;; Вывести в командную строку для контроля
    (princ (strcat "\n" str "," (rtos (car pt) 2 3) "," (rtos (cadr pt) 2 3)))
    ;; Сохраняем Y, X (y=northing, x=easting — порядок геодезии)
    (setq data (cons (list str (rtos (cadr pt) 2 3) (rtos (car pt) 2 3)) data)
          i    (1+ i)))
  (setq data    (reverse data)
        numrows (length data))
  (if (= numrows 0)
    (progn (princ "\nНет данных для таблицы.") (princ) (exit)))
  (princ (strcat "\nВыбрано " (itoa numrows) " объектов."))
  ;; Определить текущее пространство (Модель или Лист)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (if (= (getvar "TILEMODE") 1)
    ;; ---------- МОДЕЛЬ ----------
    (progn
      (princ "\nУкажите точку вставки таблицы в МОДЕЛИ: ")
      (setq inspt (getpoint))
      (if (not inspt)
        (progn (princ "\nТочка не указана.") (princ) (exit)))
      (setq mspace (vla-get-modelspace doc)))
    ;; ---------- ЛИСТ ----------
    (progn
      ;; Находимся в листе (может быть в видовом экране или на листе)
      (setq cvport (getvar "CVPORT"))
      (if (> cvport 1)
        ;; В видовом экране - переключимся на лист
        (command "_.pspace"))
      (princ "\nУкажите точку вставки таблицы в ЛИСТЕ: ")
      (setq inspt (getpoint))
      (if (not inspt)
        (progn (princ "\nТочка не указана.") (princ) (exit)))
      ;; Если указывали из видового экрана - преобразовать координаты
      (if (> cvport 1)
        (setq inspt (trans inspt 0 2)))
      (setq mspace (vla-get-paperspace doc))))
  (setq rowht    2.5
        colwidth (* (getvar "textsize") 8.0)
        tbl      (vla-addtable mspace
                   (vlax-3d-point inspt)
                   (+ numrows 1)
                   3
                   rowht
                   colwidth))
  (vla-put-stylename tbl (getvar "ctablestyle"))
  (vla-settext tbl 0 0 "Координаты характерных точек")
  (vla-settext tbl 0 1 "Y")
  (vla-settext tbl 0 2 "X")
  (setq j 1)
  (foreach rowdata data
    (vla-settext tbl j 0 (car rowdata))
    (vla-settext tbl j 1 (cadr rowdata))
    (vla-settext tbl j 2 (caddr rowdata))
    (setq j (1+ j)))
  (princ (strcat "\nТаблица создана: " (itoa numrows) " строк."))
  (princ))
; SWIFT-END

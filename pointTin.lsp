; SWIFT-START
; Swift POINTTIN v1.7 
(defun is-point-inside-polygon (pt polygon / x y n i j xi yi xj yj inside)
  (setq x      (car pt)
        y      (cadr pt)
        n      (length polygon)
        inside nil
        i      0
        j      (1- n))
  (while (< i n)
    (setq xi (car  (nth i polygon))
          yi (cadr (nth i polygon))
          xj (car  (nth j polygon))
          yj (cadr (nth j polygon)))
    (if (and (or (and (< yi y) (>= yj y))
                 (and (< yj y) (>= yi y)))
             (and (/= (- yj yi) 0.0)
                  (<= x (+ xi (* (/ (- xj xi) (- yj yi)) (- y yi))))))
      (setq inside (not inside)))
    (setq j i
          i (1+ i)))
  inside)
(if (null *ptin-rand-seed*) (setq *ptin-rand-seed* 12345))
(defun _ptin-random-between (rmin rmax / r)
  (setq *ptin-rand-seed*
        (rem (abs (+ (* *ptin-rand-seed* 1664525) 1013904223)) 4294967296))
  (setq r (/ (rem *ptin-rand-seed* 100000) 100000.0))
  (+ rmin (* r (- rmax rmin))))
(defun _ptin-valid-boundary-p (ent)
  (member (cdr (assoc 0 (entget ent)))
    '("LWPOLYLINE" "POLYLINE" "3DPOLYLINE" "AECC_FEATURE_LINE" "SPLINE" "LINE")))
(defun _ptin-curve-points (ent step / obj totalLen dist pt pts)
  (setq obj      (vlax-ename->vla-object ent)
        totalLen (vl-catch-all-apply 'vlax-curve-getDistAtParam
                   (list obj (vl-catch-all-apply 'vlax-curve-getEndParam (list obj))))
        dist     0.0
        pts      nil)
  (if (numberp totalLen)
    (progn
      (while (<= dist totalLen)
        (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj dist)))
        (if (not (vl-catch-all-error-p pt))
          (setq pts (cons pt pts)))
        (setq dist (+ dist step)))
      (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj totalLen)))
      (if (not (vl-catch-all-error-p pt))
        (setq pts (cons pt pts))))
    (princ "\nОшибка получения длины кривой."))
  (reverse pts))
(defun c:POINTTIN
    (/ *error* surfaces surface ss ptList pt elev ptData x y z
       dcl_id surfaceNames selectedIndex result surfaceObj
       useCustomPoints pointStep randomHeight heightMin heightMax
       randomCoords coordsMin coordsMax boundary boundaryObj
       boundaryPoints minPt maxPt param endParam
       dcl_content temp_dcl fp createdPts newEnt ptWCS
       elevResult boundaryPointsWCS excludeBoundaries
       excludeBoundaryPointsList i ent boundaryPtsCreated
       excludePtsCreated excludeObj excludeParam excludeEndParam
       excludePts excludePts2d closestPt wx wy)
  (defun *error* (msg)
    (if (and dcl_id (> dcl_id 0))
      (progn (unload_dialog dcl_id) (setq dcl_id 0)))
    (if (and temp_dcl (findfile temp_dcl))
      (vl-file-delete temp_dcl))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка POINTTIN: " msg)))
    (princ))
  (setq dcl_id 0)
  (setq *ptin-rand-seed* (fix (+ (* (getvar "MILLISECS") 1337) (getvar "CPUTICKS"))))
  (setq dcl_content
"surface_select : dialog {
    label = \"POINTTIN — проецирование точек на TIN\";
    : column {
        : popup_list {
            label = \"Поверхность TIN:\";
            key   = \"surface_list\";
            width = 40;
        }
        : spacer { height = 0.5; }
        : toggle { label = \"Использовать заданные точки (выбрать из чертежа)\"; key = \"use_custom_points\"; }
        : edit_box { label = \"Шаг сетки точек:\"; key = \"point_step\"; width = 10; }
        : spacer { height = 0.5; }
        : toggle { label = \"Исключить внутренние границы (выбрать контуры)\"; key = \"use_exclude_boundaries\"; }
        : spacer { height = 0.5; }
        : toggle { label = \"Случайное смещение по высоте\";        key = \"random_height\"; }
        : edit_box { label = \"Мин. смещение высоты:\";  key = \"height_min\"; width = 10; }
        : edit_box { label = \"Макс. смещение высоты:\"; key = \"height_max\"; width = 10; }
        : spacer { height = 0.5; }
        : toggle { label = \"Случайное смещение по координатам\";   key = \"random_coords\"; }
        : edit_box { label = \"Мин. смещение координат:\"; key = \"coords_min\"; width = 10; }
        : edit_box { label = \"Макс. смещение координат:\"; key = \"coords_max\"; width = 10; }
    }
    : row {
        : button { key = \"accept\"; label = \"OK\";      is_default = true; }
        : button { key = \"cancel\"; label = \"Отмена\";  is_cancel  = true; }
    }
}
error_dialog : dialog {
    label = \"POINTTIN — ошибка\";
    : text { key = \"error_message\"; value = \"\"; width = 50; }
    ok_button;
}")
  (setq temp_dcl (vl-filename-mktemp "pointtin" (getvar "TEMPPREFIX") ".dcl"))
  (setq fp (open temp_dcl "w"))
  (write-line dcl_content fp)
  (close fp)
  (setq surfaces
        (if (ssget "_X" '((0 . "AECC_TIN_SURFACE")))
          (vl-remove-if 'listp
            (mapcar 'cadr (ssnamex (ssget "_X" '((0 . "AECC_TIN_SURFACE"))))))
          nil))
  (if (null surfaces)
    (progn
      (setq dcl_id (load_dialog temp_dcl))
      (if (new_dialog "error_dialog" dcl_id)
        (progn
          (set_tile "error_message" "Ошибка: не найдено ни одной TIN-поверхности!")
          (action_tile "accept" "(done_dialog 1)")
          (start_dialog)))
      (unload_dialog dcl_id)
      (setq dcl_id 0)
      (vl-file-delete temp_dcl)
      (exit)))
  (setq surfaceNames
        (mapcar '(lambda (ent) (vla-get-Name (vlax-ename->vla-object ent))) surfaces))
  (setq dcl_id (load_dialog temp_dcl))
  (if (< dcl_id 0)
    (progn
      (vl-file-delete temp_dcl)
      (princ "\nОшибка загрузки DCL.")
      (exit)))
  (if (not (new_dialog "surface_select" dcl_id))
    (progn
      (unload_dialog dcl_id) (setq dcl_id 0)
      (vl-file-delete temp_dcl)
      (princ "\nОшибка создания диалога DCL.")
      (exit)))
  (start_list "surface_list")
  (mapcar 'add_list surfaceNames)
  (end_list)
  (set_tile "surface_list" "0")
  (set_tile "use_custom_points"      (cond ((getenv "POINTTIN_USE_CUSTOM")   (getenv "POINTTIN_USE_CUSTOM"))   (t "1")))
  (set_tile "point_step"             (cond ((getenv "POINTTIN_STEP")        (getenv "POINTTIN_STEP"))        (t "10")))
  (set_tile "use_exclude_boundaries" (cond ((getenv "POINTTIN_USE_EXCLUDE") (getenv "POINTTIN_USE_EXCLUDE")) (t "0")))
  (set_tile "random_height"          (cond ((getenv "POINTTIN_RND_H")       (getenv "POINTTIN_RND_H"))       (t "0")))
  (set_tile "height_min"             (cond ((getenv "POINTTIN_H_MIN")       (getenv "POINTTIN_H_MIN"))       (t "-1.0")))
  (set_tile "height_max"             (cond ((getenv "POINTTIN_H_MAX")       (getenv "POINTTIN_H_MAX"))       (t "1.0")))
  (set_tile "random_coords"          (cond ((getenv "POINTTIN_RND_XY")      (getenv "POINTTIN_RND_XY"))      (t "0")))
  (set_tile "coords_min"             (cond ((getenv "POINTTIN_XY_MIN")      (getenv "POINTTIN_XY_MIN"))      (t "-1.0")))
  (set_tile "coords_max"             (cond ((getenv "POINTTIN_XY_MAX")      (getenv "POINTTIN_XY_MAX"))      (t "1.0")))
  (action_tile "accept"
    "(setq useCustomPoints       (get_tile \"use_custom_points\")
           pointStep             (get_tile \"point_step\")
           useExcludeBoundaries  (get_tile \"use_exclude_boundaries\")
           randomHeight          (get_tile \"random_height\")
           heightMin             (get_tile \"height_min\")
           heightMax             (get_tile \"height_max\")
           randomCoords          (get_tile \"random_coords\")
           coordsMin             (get_tile \"coords_min\")
           coordsMax             (get_tile \"coords_max\")
           selectedIndex         (get_tile \"surface_list\"))
     (setenv \"POINTTIN_USE_CUSTOM\" useCustomPoints)
     (setenv \"POINTTIN_STEP\"       pointStep)
     (setenv \"POINTTIN_USE_EXCLUDE\" useExcludeBoundaries)
     (setenv \"POINTTIN_RND_H\"      randomHeight)
     (setenv \"POINTTIN_H_MIN\"      heightMin)
     (setenv \"POINTTIN_H_MAX\"      heightMax)
     (setenv \"POINTTIN_RND_XY\"     randomCoords)
     (setenv \"POINTTIN_XY_MIN\"     coordsMin)
     (setenv \"POINTTIN_XY_MAX\"     coordsMax)
     (done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq result (start_dialog))
  (unload_dialog dcl_id)
  (setq dcl_id 0)
  (vl-file-delete temp_dcl)
  (if (/= result 1)
    (progn (princ "\nОтмена.") (exit)))
  (setq surface    (nth (atoi selectedIndex) surfaces)
        surfaceObj (vlax-ename->vla-object surface))
  (princ (strcat "\nВыбрана поверхность: " (vla-get-Name surfaceObj)))
  (if (= useCustomPoints "1")
    (progn
      (princ "\nВыберите точки (POINT): ")
      (setq ss (ssget '((0 . "POINT"))))
      (if (null ss)
        (progn (princ "\nТочки не выбраны.") (exit)))
      (setq ptList (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))))
    (progn
      (setq pointStep (atof pointStep))
      (if (or (null pointStep) (<= pointStep 0))
        (progn (princ "\nНеверный шаг точек.") (exit)))
      (setq boundary nil)
      (while (not boundary)
        (setq boundary (car (entsel "\nВыберите внешний контур: ")))
        (if (or (null boundary) (not (_ptin-valid-boundary-p boundary)))
          (progn
            (princ "\nНеподдерживаемый тип контура.")
            (setq boundary nil))))
      (setq excludeBoundaries nil)
      (if (= useExcludeBoundaries "1")
        (progn
          (princ "\nВыберите внутренние контуры для исключения (можно несколько или Escape): ")
          (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,3DPOLYLINE,AECC_FEATURE_LINE,SPLINE,LINE"))))
          (if (not (null ss))
            (progn
              (setq i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i))
                (if (_ptin-valid-boundary-p ent)
                  (setq excludeBoundaries (cons ent excludeBoundaries)))
                (setq i (1+ i)))
              (setq excludeBoundaries (reverse excludeBoundaries))
              (princ (strcat "\nВнутренних контуров: " (itoa (length excludeBoundaries))))))))
      (setq boundaryObj      (vlax-ename->vla-object boundary)
            boundaryPointsWCS nil
            boundaryPoints    nil
            param             0
            endParam          (vl-catch-all-apply 'vlax-curve-getEndParam (list boundaryObj)))
      (if (numberp endParam)
        (progn
          (while (<= param endParam)
            (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtParam (list boundaryObj param)))
            (if (not (vl-catch-all-error-p pt))
              (setq boundaryPointsWCS (cons pt boundaryPointsWCS)))
            (setq param (1+ param)))
          (setq boundaryPointsWCS (reverse boundaryPointsWCS))
          (if (null boundaryPointsWCS)
            (progn (princ "\nОшибка: не удалось получить точки границы.") (exit)))
          (setq boundaryPoints (mapcar '(lambda (p) (trans p 0 1)) boundaryPointsWCS))
          (setq boundaryPoints (mapcar '(lambda (p) (list (car p) (cadr p))) boundaryPoints)))
        (progn (princ "\nОшибка получения параметра кривой.") (exit)))
      (setq excludeBoundaryPointsList nil)
      (foreach excludeBnd excludeBoundaries
        (setq excludeObj (vlax-ename->vla-object excludeBnd)
              excludeParam 0
              excludeEndParam (vl-catch-all-apply 'vlax-curve-getEndParam (list excludeObj))
              excludePts nil)
        (if (numberp excludeEndParam)
          (progn
            (while (<= excludeParam excludeEndParam)
              (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtParam (list excludeObj excludeParam)))
              (if (not (vl-catch-all-error-p pt))
                (setq excludePts (cons pt excludePts)))
              (setq excludeParam (1+ excludeParam)))
            (setq excludePts (reverse excludePts))
            (setq excludePts2d (mapcar '(lambda (p) (trans p 0 1)) excludePts))
            (setq excludePts2d (mapcar '(lambda (p) (list (car p) (cadr p))) excludePts2d))
            (setq excludeBoundaryPointsList (cons excludePts2d excludeBoundaryPointsList)))))
      (setq minPt (list (apply 'min (mapcar 'car  boundaryPoints))
                        (apply 'min (mapcar 'cadr boundaryPoints)))
            maxPt (list (apply 'max (mapcar 'car  boundaryPoints))
                        (apply 'max (mapcar 'cadr boundaryPoints))))
      (princ (strcat "\nОбласть: X[" (rtos (car minPt) 2 2) ".." (rtos (car maxPt) 2 2)
                     "] Y[" (rtos (cadr minPt) 2 2) ".." (rtos (cadr maxPt) 2 2)
                     "] шаг=" (rtos pointStep 2 2)))
      (setq createdPts nil
            x (car minPt))
      (while (<= x (car maxPt))
        (setq y (cadr minPt))
        (while (<= y (cadr maxPt))
          (if (and (is-point-inside-polygon (list x y) boundaryPoints)
                   (not (vl-some '(lambda (excludePoly)
                                    (is-point-inside-polygon (list x y) excludePoly))
                                 excludeBoundaryPointsList)))
            (progn
              (setq ptWCS (trans (list x y 0.0) 1 0))
              (setq newEnt (vl-catch-all-apply 'entmakex
                             (list (list '(0 . "POINT") (cons 10 ptWCS)))))
              (if (and newEnt (not (vl-catch-all-error-p newEnt)))
                (setq createdPts (cons newEnt createdPts)))))
          (setq y (+ y pointStep)))
        (setq x (+ x pointStep)))
      (setq boundaryPtsCreated (_ptin-curve-points boundary pointStep))
      (if boundaryPtsCreated
        (progn
          (foreach bpt boundaryPtsCreated
            (setq newEnt (vl-catch-all-apply 'entmakex
                            (list (list (cons 0 "POINT") (cons 10 bpt)))))
            (if (and newEnt (not (vl-catch-all-error-p newEnt)))
              (setq createdPts (cons newEnt createdPts))))
          (princ (strcat "\nТочек внешней границы: " (itoa (length boundaryPtsCreated))))))
      (foreach excludeBnd excludeBoundaries
        (setq excludePtsCreated (_ptin-curve-points excludeBnd pointStep))
        (if excludePtsCreated
          (foreach bpt excludePtsCreated
            (setq newEnt (vl-catch-all-apply 'entmakex
                            (list (list (cons 0 "POINT") (cons 10 bpt)))))
            (if (and newEnt (not (vl-catch-all-error-p newEnt)))
              (setq createdPts (cons newEnt createdPts))))))
      (setq ptList (reverse createdPts))
      (if ptList
        (princ (strcat "\nСоздано временных точек: " (itoa (length ptList))))
        (princ "\nНе создано ни одной точки."))
    ))
  (if (null ptList)
    (progn (princ "\nТочек нет в списке.") (exit)))
  (princ (strcat "\nПроецируется точек: " (itoa (length ptList))))
  (if (not (vlax-method-applicable-p surfaceObj 'FindElevationAtXY))
    (progn (princ "\nМетод FindElevationAtXY недоступен для данной поверхности.") (exit)))
  (foreach pt ptList
    (setq ptData   (entget pt)
          ptCoords (cdr (assoc 10 ptData))
          wx       (car ptCoords)
          wy       (cadr ptCoords))
    (setq elevResult (vl-catch-all-apply 'vlax-invoke
                       (list surfaceObj 'FindElevationAtXY wx wy)))
    (if (not (vl-catch-all-error-p elevResult))
      (progn
        (setq z elevResult)
        (entmod (subst (cons 10 (list wx wy z))
                       (assoc 10 ptData) ptData)))
      (progn
        (setq closestPt (vl-catch-all-apply 'vlax-invoke
                          (list surfaceObj 'FindClosestPointOnSurface wx wy 0.0)))
        (if (not (vl-catch-all-error-p closestPt))
          (progn
            (setq wx (car closestPt)
                  wy (cadr closestPt)
                  z  (caddr closestPt))
            (entmod (subst (cons 10 (list wx wy z))
                           (assoc 10 ptData) ptData)))
          (progn
            (princ (strcat "\nВне поверхности и не найдено ближайшей точки: ("
                           (rtos wx 2 2) "; " (rtos wy 2 2) ")"))
            (if (assoc 62 ptData)
              (entmod (subst (cons 62 1) (assoc 62 ptData) ptData))
              (entmod (append ptData (list (cons 62 1))))))))))
  (if (or (= randomCoords "1") (= randomHeight "1"))
    (progn
      (foreach pt ptList
        (setq ptData (entget pt)
              x      (car   (cdr (assoc 10 ptData)))
              y      (cadr  (cdr (assoc 10 ptData)))
              z      (caddr (cdr (assoc 10 ptData))))
        (if (= randomCoords "1")
          (setq x (+ x (_ptin-random-between (atof coordsMin) (atof coordsMax)))
                y (+ y (_ptin-random-between (atof coordsMin) (atof coordsMax)))))
        (if (and (= randomHeight "1") (not (assoc 62 ptData)))
          (setq z (+ z (_ptin-random-between (atof heightMin) (atof heightMax)))))
        (entmod (subst (cons 10 (list x y z)) (assoc 10 ptData) ptData)))
      (princ (strcat "\nСмещение применено: " (itoa (length ptList)) " точек.")))
    (princ "\nСмещение не применялось."))
  (if ptList
    (princ (strcat "\nГотово. Обработано точек: " (itoa (length ptList))))
    (princ "\nГотово. Обработано точек: 0"))
  (princ))
(princ "\nSwift POINTTIN v1.4 загружен. Команда: POINTTIN")
(princ)
; SWIFT-END

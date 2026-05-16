; SWIFT-START
; v 1.2 - 2026-05-09
; Автор: SWIFT (https://t.me/LispGeo)
; Описание: Поворачивает выбранные объекты на заданный угол.
;           Режимы работы: задать угол вручную или подобрать угол поворота,
;           рандомизация (50% вероятность), либо выровнять по текущей ПСК.
; Команды:
; - TurnObjects / ПоворотОбъектов: выбирает выбранные объекты, задаёт угол / рандомизацию / ПСК/угол.
(defun C:TurnObjects ( / ss i ent edata objname cen obj dcl_id angle_str angle_rad angle randomize seed modulus multiplier increment doc randomize_str use_ucs ucs_xdir dcl_temp dcl_file minpt maxpt)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\nВыберите объекты для поворота (Enter завершает выбор): ")
  (if (setq ss (ssget "_:L"))
    (progn
      (princ (strcat "Выбрано: " (itoa (sslength ss))))
      ;; Инициализация генератора (ЛКГ метод)
      (setq seed (getvar "DATE")
            modulus 65536
            multiplier 1103515245
            increment 12345)
      ;; Функция рандома
      (defun rand ()
        (setq seed (rem (+ (* seed multiplier) increment) modulus))
        (/ seed modulus)
      )
      ;; Загружаем defaults из переменных среды
      (setq angle_str (getenv "TurnObjects_Angle"))
      (if (not (= (type angle_str) 'STR)) (setq angle_str "180"))
      (setq randomize_str (getenv "TurnObjects_Randomize"))
      (if (not (= (type randomize_str) 'STR)) (setq randomize_str "0"))
      (setq randomize (atoi randomize_str))
      (setq use_ucs 0)
      ;; Создаём DCL для диалога
      (setq dcl_file (open (setq dcl_temp (strcat (getvar "TEMPPREFIX") "turnobjects.dcl")) "w"))
      (write-line "turnobjects : dialog {" dcl_file)
      (write-line "  label = \"Поворот объектов\";" dcl_file)
      (write-line "  : edit_box {" dcl_file)
      (write-line "    key = \"angle\";" dcl_file)
      (write-line "    label = \"Угол поворота (градусы):\";" dcl_file)
      (write-line (strcat "    value = \"" angle_str "\";") dcl_file)
      (write-line "    edit_width = 10;" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "  : toggle {" dcl_file)
      (write-line "    key = \"use_ucs\";" dcl_file)
      (write-line "    label = \"Выровнять по текущей ПСК\";" dcl_file)
      (write-line "    value = \"0\";" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "  : toggle {" dcl_file)
      (write-line "    key = \"randomize\";" dcl_file)
      (write-line "    label = \"Рандомизация (применять случайно каждый второй объект)\";" dcl_file)
      (write-line (strcat "    value = \"" randomize_str "\";") dcl_file)
      (write-line "  }" dcl_file)
      (write-line "  ok_cancel;" dcl_file)
      (write-line "}" dcl_file)
      (close dcl_file)
      ;; Загружаем DCL
      (if (>= (setq dcl_id (load_dialog dcl_temp)) 0)
        (progn
          (if (new_dialog "turnobjects" dcl_id)
            (progn
              (action_tile "angle" "(setq angle_str $value)")
              (action_tile "use_ucs" "(setq use_ucs (atoi $value))")
              (action_tile "randomize" "(setq randomize_str $value randomize (atoi $value))")
              (if (= (start_dialog) 1)
                (progn
                  (if (not (= (type angle_str) 'STR)) (setq angle_str "180"))
                  (if (not (= (type randomize_str) 'STR)) (setq randomize_str "0"))
                  ;; Сохранить значения в переменные среды
                  (setenv "TurnObjects_Angle" angle_str)
                  (setenv "TurnObjects_Randomize" randomize_str)
                  ;; Считаем угол
                  (if (= use_ucs 1)
                    (progn
                      ;; Угол оси X текущей ПСК относительно мировой (МСК)
                      (setq ucs_xdir (getvar "UCSXDIR"))
                      (setq angle_rad (atan (cadr ucs_xdir) (car ucs_xdir)))
                    )
                    (progn
                      (setq angle (atof angle_str))
                      (if (not (numberp angle)) (setq angle 180.0))
                      (setq angle_rad (* pi (/ angle 180.0)))
                    )
                  )
                  ;; Начало группы UNDO
                  (vla-StartUndoMark doc)
                  ;; Обходим объекты
                  (setq i 0)
                  (repeat (sslength ss)
                    (setq ent     (ssname ss i)
                          edata   (entget ent)
                          objname (cdr (assoc 0 edata)))
                    (setq obj (vlax-ename->vla-object ent))
                    ;; Центр вращения через bounding box
                    (cond
                      ((vl-catch-all-error-p
                         (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt)))
                       (princ (strcat "\nПропущен объект " objname " - невозможно определить центр")))
                      (t
                       (setq minpt (vlax-safearray->list minpt)
                             maxpt (vlax-safearray->list maxpt)
                             cen   (mapcar '(lambda (a b) (/ (+ a b) 2.0)) minpt maxpt))
                       ;; Применяем рандомизацию
                       (if (or (= randomize 0) (< (rand) 0.5))
                         (progn
                           (vla-rotate obj (vlax-3d-point cen) angle_rad)
                           (princ (strcat "\nПовёрнут: " objname))
                         )
                         (princ (strcat "\nПропущен (рандом): " objname))
                       )
                      )
                    )
                    (setq i (1+ i))
                  )
                  ;; Конец группы UNDO
                  (vla-EndUndoMark doc)
                  (princ (strcat "\nОбработано объектов: " (itoa (sslength ss))))
                )
                (princ "\nОтмена пользователем.")
              )
            )
            (princ "\nОшибка открытия диалога.")
          )
          (unload_dialog dcl_id)
        )
        (princ "\nОшибка загрузки DCL.")
      )
      ;; Удаляем временный DCL файл
      (vl-file-delete dcl_temp)
    )
    (princ "\nОбъекты не выбраны.")
  )
  (defun C:ПоворотОбъектов () (C:TurnObjects))
  (princ)
)
; SWIFT-END

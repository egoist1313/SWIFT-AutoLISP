(defun extract-number (str / len i ch numstr hasdot result)
  ;; Возвращает первое найденное число в строке или nil.
  (setq len (strlen str)
        i 1
        result nil)
  (while (and (<= i len) (null result))
    (setq ch (substr str i 1))
    (if (or (<= 48 (ascii ch) 57)  ; цифры
            (member ch '("-" "+"))  ; знак числа
            (= ch "."))             ; десятичная точка (начало дроби)
      (progn
        (setq numstr "")
        ;; Захватываем знак, если есть
        (if (member ch '("-" "+"))
          (progn
            (setq numstr (strcat numstr ch))
            (setq i (1+ i))
          )
        )
        ;; Собираем цифры и одну точку
        (setq hasdot nil)
        (while (and (<= i len)
                    (or (<= 48 (ascii (substr str i 1)) 57)
                        (and (= (substr str i 1) ".") (not hasdot))))
          (if (= (substr str i 1) ".")
            (setq hasdot t)
          )
          (setq numstr (strcat numstr (substr str i 1)))
          (setq i (1+ i))
        )
        ;; Пробуем преобразовать в число
        (if (and numstr
                 (not (wcmatch numstr "*[~0-9.,+-]*")) ; нет посторонних символов
                 (setq result (distof numstr)))
          nil   ; result уже получен, выйдем из while
          (setq result nil) ; если не число, продолжаем поиск
        )
      )
      (setq i (1+ i))
    )
  )
  result
)

(defun C:GRPFILTER ( / threshold condition ss i ent obj str val filtered doc groups
                     oldgroup gname ename ss-filtered )
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; 1. Ввод порогового числа
  (setq threshold (getreal "\nВведите пороговое число: "))
  (if (null threshold)
    (progn (princ "\nПороговое число не задано.") (exit))
  )

  ;; 2. Выбор условия: больше или меньше
  (initget "Greater Less")
  (setq condition (getkword "\nУсловие сравнения [Greater(больше)/Less(меньше)] <Greater>: "))
  (if (null condition)
    (setq condition "Greater")
  )

  ;; 3. Выбор текстовых объектов (TEXT, MTEXT, MULTILEADER)
  (princ "\nВыберите тексты и выноски для анализа: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT,MULTILEADER"))))
  (if (null ss)
    (progn (princ "\nНи одного подходящего объекта не выбрано.") (exit))
  )

  ;; 4. Фильтрация по числовому значению
  (setq filtered nil)
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent)
          str (vl-catch-all-apply 'vla-get-TextString (list obj)))
    ;; Если строка получена, пытаемся извлечь число
    (if (and str (not (vl-catch-all-error-p str)))
      (progn
        (setq val (extract-number str))
        (if (and val
                 (cond
                   ((= condition "Greater") (> val threshold))
                   ((= condition "Less")    (< val threshold))
                 ))
          (setq filtered (cons obj filtered))
        )
      )
    )
    (setq i (1+ i))
  )

  (if (null filtered)
    (progn
      (princ (strcat "\nНет объектов, содержащих число "
                     (if (= condition "Greater") ">" "<")
                     " " (rtos threshold 2 4)))
      (exit)
    )
  )

  ;; 5. Создание группы из отфильтрованных объектов
  (setq groups (vla-get-Groups doc))
  (setq gname (getstring T "\nВведите имя новой группы: "))
  (if (= gname "")
    (setq gname "FilteredGroup")
  )

  ;; Проверяем, существует ли группа с таким именем
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list groups gname))))
    (progn
      (initget "Yes No")
      (if (= (getkword (strcat "\nГруппа '" gname "' уже существует. Перезаписать? [Yes/No] <No>: ")) "Yes")
        (vla-delete (vla-item groups gname)) ; удаляем старую группу
        (progn
          (princ "\nОперация отменена.")
          (exit)
        )
      )
    )
  )

  ;; Формируем набор для команды group (можно и через ActiveX, но так проще для новичков)
  (setq ss-filtered (ssadd))
  (foreach obj filtered
    (ssadd (vlax-vla-object->ename obj) ss-filtered)
  )

  ;; Создаём группу через команду -group
  (command "_.-group" "_create" gname "" ss-filtered "")
  (princ (strcat "\nСоздана группа '" gname "' из " (itoa (length filtered)) " объектов, "
                 (if (= condition "Greater") "больших" "меньших") " "
                 (rtos threshold 2 4)))
  (princ)
)
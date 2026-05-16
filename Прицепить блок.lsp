(defun c:CB2T ( / source ed basept_wcs basept_ucs ss i ent ed_txt textpt_wcs textpt_ucs )
  (vl-load-com)

  ;; Выбор исходного блока
  (setq source (car (entsel "\nВыберите ИСХОДНЫЙ блок для копирования: ")))
  (if (not source)
    (prompt "\nНичего не выбрано.")
    (if (not (eq (cdr (assoc 0 (entget source))) "INSERT"))
      (prompt "\nВыбранный объект не является блоком.")
      (progn
        ;; Базовая точка блока (WCS)
        (setq ed (entget source))
        (setq basept_wcs (trans (cdr (assoc 10 ed)) (cdr (assoc 210 ed)) 0))
        ;; Перевод в текущую ПСК (UCS)
        (setq basept_ucs (trans basept_wcs 0 1))

        ;; Выбор текстов
        (princ "\nВыберите текстовые объекты, К КОТОРЫМ скопировать блок: ")
        (setq ss (ssget '((0 . "TEXT,MTEXT"))))
        (if (not ss)
          (prompt "\nНе выбрано ни одного текстового объекта.")
          (progn
            (setq i 0)
            (repeat (sslength ss)
              (setq ent (ssname ss i))
              (setq ed_txt (entget ent))
              
              ;; Безопасное получение точки вставки через DXF
              (if (and (eq (cdr (assoc 0 ed_txt)) "TEXT")
                       (not (and (= (cdr (assoc 72 ed_txt)) 0) (= (cdr (assoc 73 ed_txt)) 0))))
                (setq textpt_wcs (cdr (assoc 11 ed_txt))) ; Для выровненного TEXT
                (setq textpt_wcs (cdr (assoc 10 ed_txt))) ; Для MTEXT и стандартного TEXT
              )
              
              ;; Перевод точки текста из ECS/WCS в текущую ПСК (UCS)
              (setq textpt_wcs (trans textpt_wcs (cdr (assoc 210 ed_txt)) 0))
              (setq textpt_ucs (trans textpt_wcs 0 1))

              ;; Копирование блока
              (command "._COPY" source "" "_non" basept_ucs "_non" textpt_ucs)

              (setq i (1+ i))
            )
            (prompt (strcat "\nГотово. Блок скопирован к " (itoa i) " текстам."))
          )
        )
      )
    )
  )
  (princ)
)

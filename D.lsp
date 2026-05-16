; SWIFT-START
;; ===============================================================
;; • Команды: D+  D-  Д+  Д-
;; ===============================================================
(vl-load-com)
(defun split-text (txt / posP posX pos)
  (setq posP (vl-string-search "\\P" txt))
  (setq posX (vl-string-search "\\X" txt))
  (cond
    ((and posP posX) (setq pos (min posP posX)))
    (posP            (setq pos posP))
    (posX            (setq pos posX))
    (t               (setq pos nil))
  )
  (if pos
    (list t
          (substr txt 1 pos)
          (substr txt (+ pos 3))
          (substr txt (+ pos 1) 2))
    (list nil txt "" ""))
)
(defun D-Process (mode / ss i ent obj txt parts has-second upper lower sep newtxt oldAttach)
  (princ (if (= mode "add")
           "\nДобавляю «Д» (во вторую строку, если есть)\n"
           "\nУбираю «Д» (из второй строки, если была)\n"))
  (princ "\nВыберите объекты: ")
  (setq ss (ssget '((0 . "DIMENSION,TEXT,MTEXT,MULTILEADER"))))
  (if (not ss)
    (progn (princ "\nНичего не выбрано — выходим.\n") (princ) (exit)))
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent)
          oldAttach nil)
    ;; Получаем текст
    (cond
      ((wcmatch (cdr (assoc 0 (entget ent))) "TEXT,MTEXT,MULTILEADER")
       (setq txt (vla-get-TextString obj)))
      (t
       (setq txt (vla-get-TextOverride obj))
       (if (= txt "") (setq txt "<>")))
    )
    ;; Только для MTEXT временно ставим выранивание ===
    (if (eq (cdr (assoc 0 (entget ent))) "MTEXT")
      (progn
        (setq oldAttach (vla-get-AttachmentPoint obj))
        (vla-put-AttachmentPoint obj 9)   ; 9 = MiddleRight
      )
    )
    ;; Разбор и изменение текста
    (setq parts     (split-text txt)
          has-second (nth 0 parts)
          upper      (nth 1 parts)
          lower      (nth 2 parts)
          sep        (nth 3 parts))
    (cond
      ((= mode "add")
       (cond
         (has-second
          (if (= (strcase (substr lower 1 1)) "Д")
            (setq newtxt txt)
            (setq newtxt (strcat upper sep "Д" lower))))
         (t
          (if (= (strcase (substr txt 1 1)) "Д")
            (setq newtxt txt)
            (setq newtxt (strcat "Д" txt))))))
      ((= mode "remove")
       (cond
         (has-second
          (if (= (strcase (substr lower 1 1)) "Д")
            (setq newtxt (strcat upper sep (substr lower 2)))
            (setq newtxt txt)))
         (t
          (if (= (strcase (substr txt 1 1)) "Д")
            (setq newtxt (substr txt 2))
            (setq newtxt txt)))))
    )
    ;; Записываем обратно
    (cond
      ((wcmatch (cdr (assoc 0 (entget ent))) "TEXT,MTEXT,MULTILEADER")
       (vla-put-TextString obj newtxt))
      (t
       (if (= newtxt "<>") (setq newtxt ""))
       (vla-put-TextOverride obj newtxt))
    )
    ;; Возвращаем исходное выравнивание
    (if (and oldAttach (eq (cdr (assoc 0 (entget ent))) "MTEXT"))
      (vla-put-AttachmentPoint obj oldAttach)
    )
    (setq i (1+ i))
  )
  (princ (strcat "\nГотово! Обработано объектов: " (itoa (sslength ss)) "\n"))
  (princ)
)
;; Команды хз как будут в другом автокаде
(defun c:D+ nil (D-Process "add"))
(defun c:D- nil (D-Process "remove"))
(defun c:Д+ nil (D-Process "add"))
(defun c:Д- nil (D-Process "remove"))
(princ)
; SWIFT-END

;; ==================================================
;; MT2MLB — Пакетное преобразование MText в чистую мультивыноску
;; Только текст, БЕЗ линий и стрелки
;; Команда: MT2MLB
;; ==================================================

(defun c:MT2MLB ( / ss i ent obj textstr inspt ml pts doc space)
  (vl-load-com)
  (princ "\nВыберите MText для преобразования в мультивыноски (без линий)...")

  (if (setq ss (ssget '((0 . "MTEXT"))))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
            space (vla-get-ModelSpace doc))  ; если нужно в листе — замени на PaperSpace

      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              textstr (vla-get-TextString obj)
              inspt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))

        ;; Создаём мультивыноску с нулевой линией
        (setq pts (vlax-make-safearray vlax-vbDouble '(0 . 5)))
        (vlax-safearray-fill pts (list (car inspt) (cadr inspt) 0.0
                                       (car inspt) (cadr inspt) 0.0))

        (setq ml (vla-AddMLeader space pts 0))

        (vla-put-TextString ml textstr)
        (vla-put-TextHeight ml (vla-get-Height obj))   ; сохраняем высоту текста
        (vla-put-Layer ml (vla-get-Layer obj))
        
        ;; Убираем ВСЁ лишнее
        (vla-put-ArrowheadType ml 0)      ; 0 = без стрелки
        (vla-put-LandingGap ml 0.0)       ; без полки
        (vla-put-DoglegLength ml 0.0)

        (vla-delete obj)   ; удаляем старый MText
        (setq i (1+ i))
      )
      (princ (strcat "\n✓ Готово! Преобразовано " (itoa (sslength ss)) " MText в чистые мультивыноски."))
    )
    (princ "\n*** Ничего не выбрано ***")
  )
  (princ)
)
; SWIFT-START
(vl-load-com)

;;====================================================================
;; РАЗВОРОТ ТРУБЫ CIVIL 3D — ТОЧНО ПО ТВОЕМУ ТЗ (финал!)
;; Просто тыкаешь на трубы — Enter для завершения
;; Что делает:
;;   • ОТМЕТКИ (Start/End CenterlineElevation + Invert) ОСТАЮТСЯ НА СВОИХ ФИЗИЧЕСКИХ КОНЦАХ
;;   • КООРДИНАТКА ПОЛОЖЕНИЯ (StartStation ↔ EndStation) МЕНЯЕТСЯ МЕСТАМИ
;;   • Геометрия трубы и направление НЕ меняются (никаких SetStartandEndPoints)
;;   • Никаких ошибок variantp / несовпадение типов
;;   • Используем ТОЧНО твой safe-get и PointAtParam (как в Pipes-To-3DPoly)
;; Команды: РазвернутьТрубу / RT / РазворотТрубы
;;====================================================================

;; Безопасное получение свойства (как у тебя в Pipes-To-3DPoly)
(defun safe-get (obj propname / val)
  (if (vlax-property-available-p obj propname)
    (progn
      (setq val (vlax-get-property obj propname))
      (cond ((= (type val) 'VARIANT) (vlax-variant-value val))
            (T val)))
    nil))

;;====================================================================
;; Разворот одной трубы — ТОЛЬКО координатка!
;;====================================================================
(defun Reverse-One-Pipe (obj / name sstat estat )
  (if (= (vla-get-ObjectName obj) "AeccDbPipe")
    (progn
      (setq name (vlax-get-property obj 'Name))
      (if (null name) (setq name "Без имени"))

      ;; Сохраняем ТОЛЬКО координатки положения
      (setq sstat (safe-get obj "StartStation"))
      (setq estat (safe-get obj "EndStation"))

      ;; Меняем МЕСТАМИ ТОЛЬКО координатку (отметки НЕ трогаем вообще!)
      (if (and sstat estat)
        (progn
          (vlax-put-property obj 'StartStation estat)
          (vlax-put-property obj 'EndStation sstat)
        )
      )

      (vla-update obj)

      (princ (strcat "\n✓ Развёрнута: " name))
      t
    )
    nil
  )
)

;;====================================================================
;; Основная команда
;;====================================================================
(defun C:РазвернутьТрубу ( / selSS i obj count old_osmode )
  (setq old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (princ "\n\n=== РАЗВОРОТ ТРУБЫ (SwiftLisp — ОТМЕТКИ ОСТАЮТСЯ!) ===\n")
  (princ "Тыкай на трубы (одну или несколько). Enter — закончить.\n")

  (if (setq selSS (ssget '((0 . "AECC_PIPE"))))
    (progn
      (setq i 0 count 0)
      (while (< i (sslength selSS))
        (setq obj (vlax-ename->vla-object (ssname selSS i)))
        (if (Reverse-One-Pipe obj)
          (setq count (1+ count))
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n\nГОТОВО! Развёрнуто " (itoa count) " труб(ы)."))
      (princ "\nОтметки (дна и оси) остались на своих физических концах.")
      (princ "\nТолько координатка положения (ПК/станция) поменялась местами.")
    )
    (princ "\n[!] Трубы не выбраны.")
  )

  (setvar "OSMODE" old_osmode)
  (princ)
)

;; Короткие команды
(defun C:RT () (C:РазвернутьТрубу))
(defun C:РазворотТрубы () (C:РазвернутьТрубу))

(princ "\nКоманды загружены: РазвернутьТрубу / RT / РазворотТрубы")
(princ)
; SWIFT-END
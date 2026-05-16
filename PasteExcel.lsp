(defun c:ExcelToTableLink ( / *error* excelApp excelBook tempPath linkName pt dictionaries dataLinks dlObj )
  (vl-load-com)

  (defun *error* (msg)
    (if (and excelBook (not (vlax-object-released-p excelBook))) (vlax-release-object excelBook))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nОшибка: " msg)))
    (princ)
  )

  ;; 1. Путь к временному файлу
  (setq tempPath (strcat (getvar "TEMPPREFIX") "ACAD_LINK_DATA.xlsx"))
  (setq linkName "Excel_Buffer_Link")

  ;; 2. Работа с Excel (сохраняем буфер в файл)
  (princ "\nПодготовка данных из Excel...")
  (setq excelApp (vlax-get-or-create-object "Excel.Application"))
  (setq excelBook (vlax-invoke-method (vlax-get-property excelApp 'Workbooks) 'Add))
  (setq excelSheet (vlax-get-property excelBook 'ActiveSheet))
  
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke-method (list excelSheet 'Paste)))
    (progn
      (vlax-invoke-method excelBook 'Close :vlax-false)
      (vlax-release-object excelBook)
      (exit (princ "\nОшибка: Буфер обмена пуст!")))
  )
  
  (if (findfile tempPath) (vl-file-delete tempPath))
  (vlax-invoke-method excelBook 'SaveAs tempPath 51)
  (vlax-invoke-method excelBook 'Close :vlax-false)
  (vlax-release-object excelBook)
  (vla-put-visible excelApp :vlax-true)

  ;; 3. Создание Data Link через ActiveX (без использования команд)
  (setq dictionaries (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))))
  
  ;; Получаем или создаем словарь DATALINK
  (setq dataLinks (vl-catch-all-apply 'vla-item (list dictionaries "ACAD_DATALINK")))
  (if (vl-catch-all-error-p dataLinks)
    (setq dataLinks (vla-add dictionaries "ACAD_DATALINK"))
  )

  ;; Создаем объект связи данных
  (setq dlObj (vl-catch-all-apply 'vla-add (list dataLinks linkName)))
  (if (vl-catch-all-error-p dlObj)
    (setq dlObj (vla-item dataLinks linkName)) ; если уже есть, берем существующий
  )

  ;; Настраиваем связь
  (vla-put-DataAdapterId dlObj "AcExcelDataAdapter")
  (vla-put-ConnectionString dlObj tempPath)
  (vla-put-Description dlObj "Временная связь с Excel")

  ;; 4. Вставка таблицы
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if pt
    (progn
      (setvar "CMDECHO" 0)
      ;; Используем стандартный вызов вставки таблицы по имени связи
      (vl-cmdf "_TABLE" "_D" linkName pt)
      (setvar "CMDECHO" 1)
    )
  )

  (princ (strcat "\nГотово. Связь: " linkName))
  (princ)
)

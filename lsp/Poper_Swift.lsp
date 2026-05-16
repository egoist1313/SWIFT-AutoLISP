; SWIFT-START
; ================================================
; Swift POPER v3.2
; Без внешнего poper.dcl — всё в одном файле
; ================================================
(defun C:POPER (/ dcl_id mode_points mode_vectors mode_polyline h_scale l_scale text_height text_offset text_offset_str round_to slope_round_to slope_unit use_text_height show_lengths show_slopes draw_projections projection_color show_permille_sign result OSZ OSH PCoords original_PCoords point_count pt delta_y final_height top_point_height bottom_point_height top_point_length bottom_point_length prev_x dist original_unitmode original_dimzin original_lunits original_luprec heights temp_points selected_entities profile_polyline intersections sorted_PCoords sorted_heights i pt1 pt2 delta_x slope slope_text text_x mid_point mid_height top_point_slope bottom_point_slope filtered_entities entity_type temp_point_ent x_coords unique_entities text_entity text_content projection_point polyline_obj vertices length_text new_PCoords min_x max_x minpt maxpt axis_line axis_obj int_points param temp_dcl fp dcl_content round_to_index slope_unit_index line_color *poper-dcl*)
    (vl-load-com)
    ;; ====================== ВСТРОЕННЫЙ DCL ======================
    (setq dcl_content 
"poper_settings : dialog {
    label = \"Настройки Swift POPER\";
    : column {
        : radio_button { label = \"Режим: По точкам\"; key = \"mode_points\"; value = \"1\"; }
        : radio_button { label = \"Режим: По векторам\"; key = \"mode_vectors\"; }
        : radio_button { label = \"Режим: По вершинам полилинии\"; key = \"mode_polyline\"; }
        : edit_box { label = \"Масштаб по высоте:\"; key = \"h_scale\"; value = \"10\"; width = 10; }
        : edit_box { label = \"Масштаб по длине:\"; key = \"l_scale\"; value = \"1\"; width = 10; }
        : edit_box { label = \"Высота текста:\"; key = \"text_height\"; value = \"2.5\"; width = 10; }
        : edit_box { label = \"Отступ текста (%):\"; key = \"text_offset\"; value = \"10\"; width = 10; }
        : popup_list { label = \"Округление высоты до:\"; key = \"round_to\"; width = 10; list = \"2\\n3\"; }
        : popup_list { label = \"Округление уклона до:\"; key = \"slope_round_to\"; width = 10; list = \"0\\n1\\n2\"; }
        : popup_list { label = \"Единица уклона:\"; key = \"slope_unit\"; width = 10; list = \"Промилле\\nГрадусы\\nСоотношение сторон\\nАвто (промилле/соотношение)\"; }
        : popup_list { label = \"Цвет линий проекции:\"; key = \"projection_color\"; width = 10; list = \"По слою\\nКрасный\\nЖелтый\\nЗелёный\\nГолубой\"; }
        : toggle { label = \"Показать длины\"; key = \"show_lengths\"; value = \"1\"; }
        : toggle { label = \"Показать уклоны\"; key = \"show_slopes\"; value = \"1\"; }
        : toggle { label = \"Высота из текста\"; key = \"use_text_height\"; value = \"1\"; }
        : toggle { label = \"Провести линии проекции\"; key = \"draw_projections\"; value = \"0\"; }
        : toggle { label = \"Показать знак промилле\"; key = \"show_permille_sign\"; value = \"1\"; }
    }
    : row {
        : button { key = \"accept\"; label = \"OK\"; is_default = true; }
        : button { key = \"cancel\"; label = \"Отмена\"; is_cancel = true; }
        : button { key = \"help\"; label = \"?\"; width = 2; }
    }
}
poper_help : dialog {
    label = \"Справка по Swift POPER\";
    : boxed_column {
        label = \"Описание команды POPER\";
        scroll = true; fixed_width = true; width = 80; fixed_height = true; height = 18; alignment = top;
        : text { value = \"Команда POPER: Заполнение поперечников в AutoCAD\\n\\nОписание: Lisp создаёт поперечники, рассчитывая высоты, длины и уклоны на основе выбранных точек или объектов. Просто следуйте инструкции в командной строке\"; width = 75; height = 5; }
        : text { value = \"Режимы работы:\\n- По точкам: Указывайте точки профиля вручную. 'U' — отмена последней точки.\\n- По векторам: Пересечения вертикальных линий с полилинией профиля.\\n- По вершинам: Вершины полилинии + ось\"; width = 75; height = 5; }
    }
    ok_button;
}")
    ;; Создаём временный DCL-файл
    (setq temp_dcl (vl-filename-mktemp "poper" (getvar "TEMPPREFIX") ".dcl"))
    (setq fp (open temp_dcl "w"))
    (write-line dcl_content fp)
    (close fp)
    (setq *poper-dcl* temp_dcl)
    ;; ====================== ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ======================
    (defun calculate-height (pt OSZ h_scale OSH)
        (+ OSH (* (- (cadr pt) (cadr OSZ)) (/ 1.0 h_scale)))
    )
    ;; ... (остальная часть файла следует за этим)
    (princ)
; SWIFT-END
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
    ... [полный код диалога и функции] ...
}")
    (princ "\nSwift POPER v3.2 загружен. Команда: POPER")
; SWIFT-END
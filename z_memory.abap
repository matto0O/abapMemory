REPORT z_memory.


TYPE-POOLS: icon.

TABLES: icon, zmwo_ranking.

PARAMETERS: p_rows     TYPE i OBLIGATORY,
            p_cols     LIKE p_rows OBLIGATORY,
            p_sleep    TYPE i OBLIGATORY DEFAULT 3,
            p_name(15) TYPE c OBLIGATORY.


CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_double_click
                    FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.
ENDCLASS.


TYPES: BEGIN OF button,
         butid   TYPE i,
         rowid   TYPE i,
         colid   TYPE i,
         value   TYPE icon,
         guessed TYPE i,
       END OF button.


TYPES: BEGIN OF final,
         col1  TYPE string,
         col2  TYPE string,
         col3  TYPE string,
         col4  TYPE string,
         col5  TYPE string,
         col6  TYPE string,
         col7  TYPE string,
         col8  TYPE string,
         col9  TYPE string,
         col10 TYPE string,
         col11 TYPE string,
         col12 TYPE string,
         col13 TYPE string,
         col14 TYPE string,
         col15 TYPE string,
         col16 TYPE string,
         col17 TYPE string,
         col18 TYPE string,
       END OF final.

DATA: ls_button         TYPE button,
      lt_buttons        TYPE STANDARD TABLE OF button WITH KEY butid,
      lv_row_index      LIKE p_rows,
      lv_icon           TYPE icon,
      lt_used_icons     TYPE STANDARD TABLE OF icon,
      lv_tablen         TYPE i,
      lv_icon_index     TYPE i,
      lt_fieldcat       TYPE lvc_s_fcat OCCURS 0,
      ls_fieldcat       LIKE LINE OF lt_fieldcat,
      lv_amount         LIKE p_rows,
      ls_final          TYPE final,
      lt_final          TYPE STANDARD TABLE OF final,
      lo_event_receiver TYPE REF TO lcl_event_receiver,
      ls_flipped        LIKE ls_button,
      lv_paired         TYPE i,
      lv_tries          TYPE i.

DATA(lo_alv) = NEW cl_gui_alv_grid( i_parent = cl_gui_container=>default_screen
                                    i_appl_events = abap_true ).

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.
    DATA: field TYPE string,
          index TYPE i.

    index = e_row-index.
    READ TABLE lt_final INDEX index INTO ls_final.
    CONCATENATE '' e_column INTO field.
    ASSIGN COMPONENT field OF STRUCTURE ls_final TO FIELD-SYMBOL(<fs_icon>).

    IF sy-subrc = 0.
      LOOP AT lt_buttons INTO ls_button.
        DATA(colstr) = |col{ ls_button-colid }|.
        IF ls_button-rowid = index AND colstr = e_column.
          cl_demo_output=>write( ls_button-guessed ).
          IF ls_button-guessed IS INITIAL.
            <fs_icon> = ls_button-value-id.
            MODIFY lt_final INDEX index FROM ls_final.
            CALL METHOD lo_alv->refresh_table_display.
          ELSE.
            RETURN.
          ENDIF.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      cl_demo_output=>write( sy-subrc ).
    ENDIF.
    cl_demo_output=>display( lt_final ).

    IF ls_flipped IS INITIAL.
      ls_flipped = ls_button.
    ELSE.
      WAIT UP TO p_sleep SECONDS.
      IF ls_button-value-id = ls_flipped-value-id AND ls_flipped-value-id <> icon_system_help
          AND ( ls_button-rowid <> ls_flipped-rowid OR ls_button-colid <> ls_flipped-colid ).
        ls_button-guessed = 1.
        ls_flipped-guessed = 1.

        lv_paired = lv_paired + 1.
        lv_tries = lv_tries + 1.

        MODIFY lt_buttons INDEX ls_button-butid FROM ls_button.
        MODIFY lt_buttons INDEX ls_flipped-butid FROM ls_flipped.

        IF lv_paired = lv_amount / 2.
          cl_demo_output=>write( 'Wygrales' ).

          DATA ls_result LIKE zmwo_ranking.
          ls_result-username = p_name.
          ls_result-zday = sy-datum.
          ls_result-time = sy-uzeit.
          ls_result-score = lv_tries.
          INSERT zmwo_ranking FROM ls_result.

          cl_demo_output=>begin_section( 'Ranking' ).
          cl_demo_output=>display_data( zmwo_ranking ).
        ENDIF.
      ELSEIF ls_button-guessed IS INITIAL.
        <fs_icon> = icon_system_help.
        MODIFY lt_final INDEX index FROM ls_final.

        READ TABLE lt_final INDEX ls_flipped-rowid INTO ls_final.
        field = |col{ ls_flipped-colid }|.
        ASSIGN COMPONENT field OF STRUCTURE ls_final TO FIELD-SYMBOL(<fs_icon2>).
        <fs_icon2> = icon_system_help.
        MODIFY lt_final INDEX ls_flipped-rowid FROM ls_final.

        lv_tries = lv_tries + 1.

        WAIT UP TO p_sleep SECONDS.
        CALL METHOD lo_alv->refresh_table_display.
      ENDIF.

      CLEAR ls_flipped.
    ENDIF.

  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.

  lv_amount = p_rows * p_cols.

  IF lv_amount < 12.
    MESSAGE 'Minimalna liczba ikon to 12' TYPE 'E'.
  ELSEIF lv_amount > 30.
    MESSAGE 'Maksymalna liczba ikon to 30' TYPE 'E'.
  ELSEIF lv_amount MOD 2 <> 0.
    MESSAGE 'Liczba ikon (kolumny * wiersze) musi być parzysta' TYPE 'E'.
  ENDIF.


  lv_tablen = lv_amount / 2.
  SELECT * FROM icon INTO @lv_icon UP TO @lv_tablen ROWS.
    APPEND lv_icon TO lt_used_icons.
    APPEND lv_icon TO lt_used_icons.
  ENDSELECT.

  DO p_rows TIMES.
    lv_row_index = sy-index - 1.
    DO p_cols TIMES.
      lv_tablen = lines( lt_used_icons ).
      ls_button-butid = sy-index + p_cols * lv_row_index.
      DATA(rndm) = cl_abap_random_int=>create( min = 1 max = lv_tablen seed = lv_tablen ).
      lv_icon_index = rndm->get_next( ).
      READ TABLE lt_used_icons INDEX lv_icon_index INTO ls_button-value.
      DELETE lt_used_icons INDEX lv_icon_index.
      ls_button-rowid = lv_row_index + 1.
      ls_button-colid = sy-index.
      APPEND ls_button TO lt_buttons.
    ENDDO.
  ENDDO.

  DO p_cols TIMES.
    DATA lv_index TYPE c.
    lv_index = sy-index.
    CONCATENATE 'col' lv_index INTO ls_fieldcat-fieldname.
    ls_fieldcat-datatype = 'STRING'.
    ls_fieldcat-coltext = lv_index.
    ls_fieldcat-col_pos = sy-index - 1.
    ls_fieldcat-outputlen = 4.
    APPEND ls_fieldcat TO lt_fieldcat.
  ENDDO.

  DATA(len) = lines( lt_buttons ).

  DO len TIMES.
    READ TABLE lt_buttons INDEX sy-index INTO ls_button.
    DATA(row) = ls_button-rowid.
    DATA col TYPE c.
    col = ls_button-colid.
    IF col = 1.
      IF row <> 1.
        APPEND ls_final TO lt_final.
      ENDIF.
      CLEAR ls_final.
    ENDIF.
    DATA field TYPE string.
    CONCATENATE 'col' col INTO field.
    ASSIGN COMPONENT field OF STRUCTURE ls_final TO FIELD-SYMBOL(<fs_icon>).
    IF sy-subrc = 0.
      <fs_icon> = icon_system_help.
    ELSE.
      WRITE sy-subrc.
    ENDIF.
  ENDDO.

  APPEND ls_final TO lt_final.
  CLEAR ls_final.

  CREATE OBJECT lo_event_receiver.
  SET HANDLER lo_event_receiver->handle_double_click FOR lo_alv.

  CALL METHOD lo_alv->set_table_for_first_display
    EXPORTING
      i_structure_name = 'MEMORY'
    CHANGING
      it_fieldcatalog  = lt_fieldcat
      it_outtab        = lt_final.

  WRITE: space.

REPORT  ZABAP_ALIAS_AUTH_IN_AUTHCHECK.
TABLES:t001.
"Types
TYPES:
      BEGIN OF t_1001,
        bukrs TYPE t001-bukrs,
        butxt TYPE t001-butxt,
        ort01 TYPE t001-ort01,
        land1 TYPE t001-land1,
      END OF t_1001.
"Work area
DATA:
      w_t001 TYPE t_1001.
"Internal table
DATA:
      i_t001 TYPE STANDARD TABLE OF t_1001.

*&---------------------------------------------------------------------*
* ALV Declarations
*----------------------------------------------------------------------*
* Types Pools
TYPE-POOLS:
   slis.
* Types
TYPES:
   t_fieldcat         TYPE slis_fieldcat_alv,
   t_events           TYPE slis_alv_event,
   t_layout           TYPE slis_layout_alv.
* Workareas
DATA:
   w_fieldcat         TYPE t_fieldcat,
   w_events           TYPE t_events,
   w_layout           TYPE t_layout.
* Internal Tables
DATA:
   i_fieldcat         TYPE STANDARD TABLE OF t_fieldcat,
   i_events           TYPE STANDARD TABLE OF t_events.

*data s_mtart type RANGE OF mtart nodisplay.

*&---------------------------------------------------------------------*
*&    start of selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*  select-OPTIONS: s_matnr for mara-matnr.
  .
  parameters P_mtart type mtart NO-DISPLAY.

  parameters p_dbcon type dbcon_name.

  parameters p_num type sytabix.

  PERFORM get_data.



*&---------------------------------------------------------------------*
*&    end-of-selection.
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM build_fieldcatlog.
  PERFORM build_events.
  PERFORM build_layout.
  PERFORM list_display.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_data .

*data wa_mtart like LINE OF s_mtart.
data str type string.
data lv_len type i.
data lv_stmt type string.
data lv_row type i.
data l_sql_error type string.
data l_sql_errtxt type string.

*wa_mtart-low = p_mtart.
*wa_mtart-sign = 'EQ'.
*wa_mtart-option = 'I'.

data myuser type syuname value 'abc'.

AUTHORITY-CHECK OBJECT 'S_TCODE'
          FOR USER myuser
          ID 'TCD' FIELD 'SE38'.

*select * from mara into table @data(lt_mara) CONNECTION (p_dbcon) where (str)  .

if sy-subrc EQ 0.

  endif.


  SELECT bukrs
         butxt
         ort01
         land1
    FROM t001
    INTO TABLE i_t001
    UP TO 30 ROWS.

data wa_t001 type t_1001.
if sy-subrc NE 0.
  wa_t001-bukrs = '1000'.
  wa_t001-butxt = ' Test company'.
  wa_t001-ort01 = 'New York'.
  wa_t001-land1 = 'US'.
  append wa_t001 to i_t001.

    wa_t001-bukrs = '2000'.
  wa_t001-butxt = ' Sample company'.
  wa_t001-ort01 = 'New York'.
  wa_t001-land1 = 'US'.
  append wa_t001 to i_t001.


    wa_t001-bukrs = '3000'.
  wa_t001-butxt = 'Functional test'.
  wa_t001-ort01 = 'New York'.
  wa_t001-land1 = 'US'.
  append wa_t001 to i_t001.


    wa_t001-bukrs = '4000'.
  wa_t001-butxt = 'ABC company'.
  wa_t001-ort01 = 'New York'.
  wa_t001-land1 = 'US'.
  append wa_t001 to i_t001.


    wa_t001-bukrs = '5000'.
  wa_t001-butxt = 'Demo company'.
  wa_t001-ort01 = 'New York'.
  wa_t001-land1 = 'US'.
  append wa_t001 to i_t001.
endif.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  build_fieldcatlog
*&---------------------------------------------------------------------*
FORM build_fieldcatlog .
  CLEAR:w_fieldcat,i_fieldcat[].
*  do 3500 TIMES.
  PERFORM build_fcatalog USING:
           'BUKRS' 'I_T001' 'BUKRS',
           'BUTXT' 'I_T001' 'BUTXT',
           'ORT01' 'I_T001' 'ORT01',
           'LAND1' 'I_T001' 'LAND1'.
*enddo.


ENDFORM.                    "BUILD_FIELDCATLOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCATALOG
*&---------------------------------------------------------------------*
FORM build_fcatalog USING l_field l_tab l_text.

  w_fieldcat-fieldname      = l_field.
  w_fieldcat-tabname        = l_tab.
  w_fieldcat-seltext_m      = l_text.
  w_fieldcat-outputlen      = 100.

  APPEND w_fieldcat TO i_fieldcat.
  CLEAR w_fieldcat.

ENDFORM.                    " build_fieldcatlog
*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_events.
*  CLEAR :
*        w_events, i_events[].
*  w_events-name = 'TOP_OF_PAGE'."Event Name
*  w_events-form = 'TOP_OF_PAGE'."Callback event subroutine
*  APPEND w_events TO i_events.
*  CLEAR  w_events.

ENDFORM.                    "build_events
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM build_layout .

  w_layout-colwidth_optimize = 'X'.
  w_layout-zebra             = 'X'.

ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  list_display
*&---------------------------------------------------------------------*
FORM list_display .
  DATA:
        l_program TYPE sy-repid.
  l_program = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_program
      is_layout          = w_layout
      it_fieldcat        = i_fieldcat
      it_events          = i_events
    TABLES
      t_outtab           = i_t001
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " list_display
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA :
   li_header TYPE slis_t_listheader,
   w_header  LIKE LINE OF li_header.
  DATA:
        l_date TYPE char10.
  WRITE sy-datum TO l_date.
  w_header-typ  = 'H'.
  CONCATENATE sy-repid ':' 'From Date' l_date INTO w_header-info SEPARATED BY space.
  APPEND w_header TO li_header.
  CLEAR w_header.

  w_header-typ  = 'S'.
  w_header-info = sy-title.
  APPEND w_header TO li_header.
  CLEAR w_header.

  w_header-typ  = 'A'.
  w_header-info = sy-uname.
  APPEND w_header TO li_header.
  CLEAR w_header.

  ENDFORM.

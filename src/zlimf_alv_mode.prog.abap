*&---------------------------------------------------------------------*
*& REPORT ZLIMF_ALV_MODE.
*&---------------------------------------------------------------------*
*& Program Name         <程序名称>: ZLIMF_ALV_MODE.
*& Purpose              <程序用途>:
*& Project Name         <项目名称>:
*& Created by           <创 建 人>:
*& Create on            <创建日期>:
*& Functional Consultant<功能顾问>:
*& Description          <功能描述>:
*&
*&---------------------------------------------------------------------*
*              Modification Log<程序修改日志>
*<日期>        <开发者>     <功能顾问>            <修改描述>
* Date        Programmer     Corr. #              请求号+描述
*                           FI Consultant
REPORT zlimf_alv_mode.


TYPE-POOLS: slis.
TYPE-POOLS: kcde.

*&---------------------------------------------------------------------*
*&      TABLES
*&---------------------------------------------------------------------*
TABLES: spfli. "参考表

*----------------------------------------------------------------------*
* GLOBAL INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
DATA: gt_file TYPE filetable.

*----------------------------------------------------------------------*
* GLOBAL VARIANTS DECLARATION
*----------------------------------------------------------------------*
DATA: g_repid TYPE sy-repid.
DATA: gt_field TYPE slis_t_fieldcat_alv.
DATA: go_grid TYPE REF TO cl_gui_alv_grid.
*定义读入EXCEL的内表

TYPES : BEGIN OF typ_alv ,
          carrid    TYPE spfli-carrid    , "航线代码
          connid    TYPE spfli-connid    , "航班连接编号
          countryfr TYPE spfli-countryfr , "国家/地区代码
          cityfrom  TYPE spfli-cityfrom  , "起飞城市
          airpfrom  TYPE spfli-airpfrom  , "始发机场
          countryto TYPE spfli-countryto , "国家/地区代码
          cityto    TYPE spfli-cityto    , "到达城市
          airpto    TYPE spfli-airpto    , "目标机场
          x         TYPE char20, "自定义修改字段
          y         TYPE char20, "随动字段
          sel       TYPE      c, "选择标记
          styl      TYPE lvc_t_styl, "，可以编辑修改
        END OF typ_alv .
DATA : gs_alv TYPE typ_alv,
       gt_alv TYPE TABLE OF typ_alv.

*创建字段宏定义
DEFINE add_field.
  ls_fieldcat-fieldname = '&1'."字段名称
  ls_fieldcat-ref_table = &2. "关联表格
  ls_fieldcat-ref_field = &3. "关联字段
  ls_fieldcat-coltext   = &4. "描述文本
  ls_fieldcat-edit      = &5. "列修改标识
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& 类定义
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD data_changed .
    DATA ls_stbl TYPE lvc_s_stbl.

    PERFORM frm_data_changed USING er_data_changed.

    ls_stbl-row = 'X'." 基于行的稳定刷新
    ls_stbl-col = 'X'." 基于列稳定刷新
    CALL METHOD go_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stbl.
  ENDMETHOD.
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA go_event_receiver TYPE REF TO lcl_event_receiver .


*--------选择条件
SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:s_carrid  FOR     spfli-carrid . "航线代码


SELECTION-SCREEN END OF BLOCK bl01.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : p_wwc  AS CHECKBOX DEFAULT 'X'."一个样式
SELECTION-SCREEN END OF BLOCK blk2.


* ALV 用
DATA:
  gs_layout   TYPE lvc_s_layo,
  gv_repid    TYPE repid,
  gt_fieldcat TYPE lvc_t_fcat,
  ls_fieldcat TYPE lvc_S_fcat.

*DATA: GT_ALVDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.
*DATA: G_LIGHTS_NAME TYPE LVC_CIFNM VALUE 'LIGHT'.
*&---------------------------------------------------------------------*
*&   EVENT AT INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION .

AT SELECTION-SCREEN OUTPUT .

*----------------------------------------------------------------------*
* EVENT OCCURS AFTER THE SELECTION SCREEN HAS BEEN PROCESSED
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM frm_auth_check.  "权限检查
  PERFORM frm_getdata .    "获取数据  --GETTING DATA
  PERFORM frm_display_data.  "展示ALV

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form frm_auth_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_auth_check .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_getdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata .
  DATA: fieldstyle TYPE lvc_s_styl.
*数据取值
  SELECT *
    FROM spfli
   WHERE spfli~carrid  IN @s_carrid "航线代码
    INTO CORRESPONDING FIELDS OF TABLE @gt_alv.
  IF sy-subrc = 0.
    SORT  gt_alv BY carrid connid.

    "循环处理数据
    LOOP AT   gt_alv INTO gs_alv.

      IF gs_alv-connid = '0001'."航班id为 1不让修改
        fieldstyle-fieldname = 'X'.  " 需要编辑的列名
        fieldstyle-style = cl_gui_alv_grid=>mc_style_disabled. " 设置为不可编辑状态
        APPEND fieldstyle TO gs_alv-styl.
      ENDIF.

      MODIFY gt_alv FROM gs_alv.
      CLEAR:gs_alv.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_data .
  DATA : lv_html_header TYPE slis_formname  .
*定义输出模式
  CLEAR:gs_layout,gt_fieldcat.
  gs_layout-cwidth_opt    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-box_fname     = 'SEL'.
  gs_layout-stylefname    = 'STYL'."单元格样式
  gv_repid = sy-repid.
  PERFORM frm_set_fieldcat.
  DATA(lt_events) = VALUE slis_t_event( ( name = 'CALLER_EXIT' form = 'FRM_ALV_EVENTS' ) ).

*  TRY .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      is_layout_lvc            = gs_layout
      it_events                = lt_events "事件注册
 "    I_CALLBACK_HTML_TOP_OF_PAGE = 'HTML_TOP_OF_PAGE'
*     I_CALLBACK_HTML_END_OF_LIST = 'HTML_TOP_OF_PAGE'
      it_fieldcat_lvc          = gt_fieldcat
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  add_field:
          carrid    'SPFLI'   'CARRID   '  '航线代码    '    '',
          connid    'SPFLI'   'CONNID   '  '航班连接编号  '  '',
          countryfr 'SPFLI'   'COUNTRYFR'  '国家/地区代码 '  '',
          cityfrom  'SPFLI'   'CITYFROM '  '起飞城市    '    '',
          airpfrom  'SPFLI'   'AIRPFROM '  '始发机场    '    '',
          countryto 'SPFLI'   'COUNTRYTO'  '国家/地区代码 '  '',
          cityto    'SPFLI'   'CITYTO   '  '到达城市    '    '', "最后一列可以修改
          airpto    'SPFLI'   'AIRPTO'     '目标机场'    '',
           x        ''         ''       '自定义修改字段'  'X',
          y         ''         ''        '随动字段'       ''.
ENDFORM.

FORM frm_pf_status USING extab TYPE slis_t_extab.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  CLEAR:fcode[].
  "APPEND '某按钮名称' to   fcode.
  SET PF-STATUS 'STANDARD' EXCLUDING fcode[] . "双击创建状态，在里面添加按钮
ENDFORM.

*自定义用户动作
FORM frm_user_command USING p_ucomm TYPE sy-ucomm rs_selfield TYPE slis_selfield.
  DATA : l_ucomm TYPE sy-ucomm  .
  IF go_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = go_grid.
  ENDIF.

  CALL METHOD go_grid->check_changed_data.
  rs_selfield-refresh     = 'X'.
  rs_selfield-col_stable  = 'X'.
  rs_selfield-row_stable  = 'X'.
  l_ucomm = p_ucomm .
  CLEAR p_ucomm .

  CASE l_ucomm.

    WHEN '&IC1'. "双击跳转事务事务代码
      READ TABLE gt_alv INDEX rs_selfield-tabindex INTO DATA(wa_sel_temp).  "获取击行
      IF sy-subrc = 0 .
        MESSAGE '双击了，航班代码为:' &&  wa_sel_temp-carrid TYPE  'I'.
      ENDIF.
      CLEAR:wa_sel_temp.
    WHEN 'ZBU'.
      MESSAGE '点了自定义按钮' TYPE  'I'.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

FORM frm_alv_events USING is_grid TYPE slis_data_caller_exit..
  IF go_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = go_grid.
  ENDIF.
  "回车 或者 失去焦点触发更新内表
  CALL METHOD go_grid->register_edit_event     "注册GRID事件
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter "回车    mc_evt_modified "单元格失去焦点
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CREATE OBJECT go_event_receiver.
  SET HANDLER go_event_receiver->data_changed FOR go_grid.

ENDFORM.

"修改单元格的后的
FORM frm_data_changed  USING io_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  FIELD-SYMBOLS: <tab>          TYPE STANDARD TABLE.
  DATA:  lt_alv_mod    TYPE TABLE OF typ_alv.

  ASSIGN io_data_changed->mp_mod_rows->* TO <tab>.
  MOVE-CORRESPONDING <tab> TO lt_alv_mod."获得修改后的数据，gt_alv还是修改前的数据

  "修改了X-自动修改Y
  LOOP AT io_data_changed->mt_mod_cells INTO DATA(ls_cells) WHERE fieldname = 'X'.
    READ TABLE gt_alv  ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX ls_cells-row_id.

    "报错,不准修改为曹县，不然报错
    IF ls_cells-value = '曹县'  .
      io_data_changed->add_protocol_entry(
          i_msgid     = '00'
          i_msgty     = 'E'
          i_msgno     = 001
          i_msgv1     = '请勿修改为曹县'
          i_fieldname = 'X'
          i_row_id    = ls_cells-row_id
             ).
    ELSE.
      <fs_alv>-y = ls_cells-value ."修改了X-自动修改Y
    ENDIF.

    CLEAR:ls_cells.
  ENDLOOP.

ENDFORM.

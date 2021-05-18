*&---------------------------------------------------------------------*
*& REPORT ZLIMF_WRITE_LONG_TEXT.
*&---------------------------------------------------------------------*
*& Program Name         <程序名称>: ZLIMF_WRITE_LONG_TEXT.
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
REPORT ZLIMF_WRITE_LONG_TEXT.


DATA:ls_theader TYPE thead, "长文本的head
           lt_lines   TYPE TABLE OF tline,
           ls_lines   TYPE tline.

          ls_theader-tdobject = 'EBAN'."固定值
          ls_theader-tdname = '130000010000010'."物料编码（如果有前导零的话，一定要补上）
          ls_theader-tdid = 'B01'."固定值
          ls_theader-tdspras = sy-langu.

          ls_lines-tdformat = '*'.
          ls_lines-tdline = '999'.
          APPEND ls_lines TO lt_lines.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              client          = sy-mandt
              header          = ls_theader
*             INSERT          = ' '
              savemode_direct = 'X'
*             OWNER_SPECIFIED = ' '
*             LOCAL_CAT       = ' '
*             KEEP_LAST_CHANGED       = ' '
*           IMPORTING
*             FUNCTION        =
*             NEWHEADER       =
            TABLES
              lines           = lt_lines
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

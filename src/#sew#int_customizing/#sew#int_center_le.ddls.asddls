@AbapCatalog.sqlViewName: '/SEW/INT_V_CLE'
@AbapCatalog.preserveKey: true
@AbapCatalog.buffering.status: #ACTIVE
@AbapCatalog.buffering.type: #FULL
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ClientHandling.type: #CLIENT_DEPENDENT
@VDM.viewType: #CONSUMPTION
@EndUserText.label: 'SEW Integration Center PA/OM Union'
define view /SEW/INT_CENTER_LE as 
select from /SEW/INT_CENTER_UN as un
    inner join /sew/int_msg_l as msg     on un.aend_id   = msg.aend_id  
    left outer to one join pa0001 as p1  on un.otype     = 'P' 
                                        and un.sap_id    != '00000000'
                                        and un.run_date  >= p1.begda 
                                        and un.run_date  <= p1.endda 
                                        and un.sap_id    = p1.pernr
                                        and p1.sprps     = ''  
    left outer to one join hrp1000 as h1 on un.otype     = 'O' 
                                        and un.sap_id    != '00000000'
                                        and un.run_date  >= h1.begda 
                                        and un.run_date  <= h1.endda
                                        and un.sap_id    = h1.objid
                                        and h1.otype     = 'O' 
                                        and h1.plvar     = '01'
                                        and h1.istat     = '1'
                                        and h1.langu     = 'E' {
    key un.mandt,
    key un.aend_id,
    key msg.msg_guid                            as msg_id,
    int_run,
    timestamp,
    run_date,
    sap_id,
    cloud_id,
    molga,
    un.otype,
    un.infty,
    un.subty,
    un.objps,
    un.sprps,
    un.istat,
    priox,
    un.endda,
    un.begda,
    un.seqnr,
    varyf,
    operation,
    un.uname,
    sachb_sbgrp,
    sachb_code,
    sachb_sbgrpcode,
    un.aedtm,
    un.status,
    stream_direction,   
    cast( case 
        when p1.ename is not null then p1.ename 
        when h1.stext is not null then h1.stext 
        else '- No Name -'
    end as abap.char( 40 ) )                    as sap_name, 
    msg.aedtm                                   as msg_aedtm,
    msg.uname                                   as msg_uname,
    msg.type                                    as msg_type,
    msg.id                                      as msg_class,
    msg.num                                     as msg_num,
    message                                     as msg_text,
    message_v1                                  as msg_v1,
    message_v2                                  as msg_v2,
    message_v3                                  as msg_v3,
    message_v4                                  as msg_v4,
    concat_with_space(id, num, 1)               as msg_idnum, 
    msg.status                                  as msg_status,
    cast( case 
        when msg.status = '01' then 1
        when msg.status = '02' then 3
        when msg.status = '03' then 2
        else 0
    end as int1 )                               as criticality_4_msgstatus,  
    cast( case 
        when type = 'E' then 1      //ERROR
        when type = 'A' then 1      //ERROR
        when type = 'W' then 2      //WARNING
        when type = 'S' then 3      //SUCCESS
        else 0                      //NEUTRAL
    end as int1 )                               as criticality_4_type,
    counter_chg,
    counter_chg_e,
    criticality_4_status,  
    cast( case 
        when msg.type is not null then 1 
        else 0
    end as abap.int4 )                          as counter_msg,
    cast( case 
        when type = 'E' or type = 'A' then 1
        else 0
    end as abap.int4 )                          as counter_msg_e
} 

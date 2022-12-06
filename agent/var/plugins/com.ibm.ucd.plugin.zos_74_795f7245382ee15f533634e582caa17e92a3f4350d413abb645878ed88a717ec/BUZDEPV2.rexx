/* REXX */
/*%STUB CALLCMD*/
/*********************************************************************/
/*                                                                   */
/*                                                                   */
/*                                                                   */
/* Copyright:    Licensed Materials - Property of IBM and/or HCL     */
/*                                                                   */
/*            "Restricted Materials of IBM and HCL"                  */
/*                                                                   */
/*               5725-M54                                            */
/*                                                                   */
/*               Copyright HCL Tech.                                 */
/*               All rights reserved                                 */
/*                                                                   */
/*               US Government Users Restricted Rights -             */
/*               Use, duplication or disclosure restricted by        */
/*               GSA ADP Schedule Contract with IBM Corp.            */
/*                                                                   */
/*********************************************************************
 PROGRAM NAME        - BUZDEPV2

 PROGRAM DESCRIPTION - This program is written for performing
                         1) Deployment of Modules into PDS Library
                         2) Deletion of Modules from PDS Library
                         3) Deployment of Sequential dataset and
                         4) Deletion of Sequential dataset
********************************************************************

Functions and their definitions

 1.  Load_Msg_Ids     -  To load Message Ids for the failure
 2.  ExitProc         -  To exit the program with passed RC
 3.  Delete_Seq       -  Delete Sequetial file
 4.  Delete_Pds_Mem   -  Delete member of a PDS file
 5.  Update_Seq       -  Update/Overwrite Deploy Sequential file
 6.  Update_Load_Pds  -  Add/Update all load modules in Deploy DSN
 7.  Update_Pds       -  Add/Update PDS members in Deploy DSN
 8.  check_format     -  To check format of action before processing
 9.  CleanUpWorkFiles -  Clean Up Working Files like Action file
10.  ErrorHandling    -  Handling Error for a process and throw a RC

Variable and their definitions

     ActnDSN    - Action Dataset

     DSNtype    - DSN Type like SEQ or PDS
     Action     - Action to be performed like Update/Delete
     SrceDSN    - Temporary/Source Dataset name
     TargDSN    - Deploy/Target Dataset name
     PDSmem     - PDS Member name
     DepType    - Deploy Type like LOAD/COBOL/DBRM/REXX/JCL
     MDate      - Modification Date
     MUser      - Modification User

********************************************************************/
parse arg Module '"'ActnDSN'"'

ActnDSN = STRIP(ActnDSN)
/* say "ActnDSN " ActnDSN */

elapsed = Time('E')
say ""
say "   ******   Start of Deployment Process   ******   "
say ""

/* Trace if needed
TraceCmd  = 'Trace a'
Interpret TraceCmd */

/* Initialize errors into a stem variable for handling errors */
call Load_Msg_Ids

/* Check if Action Data set is exist */
if SYSDSN("'"ActnDSN"'") <> "OK" then
   call ErrorHandling(ActnDSN 8 msgId.10)

/* Read Action statements into a stem variable */

Address TSO "ALLOC F(ACTDSN)  DA('"ActnDSN"') SHR REUSE"
drop Actlist.

Address TSO "EXECIO * DISKR ACTDSN (STEM Actlist. FINIS)"
exec_RC = rc

 x = Msg('off')
     Address TSO "FREE F(ACTDSN)"
 x = Msg('on')

/* In case of any problem with EXECIO operation, exit with error */
if exec_RC <> 0 then call ErrorHandling(ActnDSN exec_RC msgId.5)

/* Main Process */
/* Reading Action dataset records one by one and process it */
/* Note : The offset starts from 2nd line as 1st line is header */
do offset = 2 to Actlist.0
   call check_format
   parse upper var Actlist.offset DSNtype ";" Action ";" SrceDSN ";",
               TargDSN ";" PDSmem ";" DepType ";" MDate ";" MUser .

        DSNtype =  STRIP(DSNtype)
        Action  =  STRIP(Action)
        SrceDSN =  STRIP(SrceDSN)
        TargDSN =  STRIP(TargDSN)
        PDSmem  =  STRIP(PDSmem)
        DepType =  STRIP(DepType)
        MDate   =  STRIP(MDate)
        MUser   =  STRIP(MUser)

  /* Identify Action to be performed and DSN Type from Action record */
   Select
      when ( Action == "UPDATE" & DSNtype == "PDS" ) then do
         BUZMARG1 = TargDSN
         Address ISPEXEC 'GETMSG MSG(BUZD138) LONGMSG(BUZLNGER)'
         Say BUZLNGER
         if ( DepType <> "TEXT" ) then
            call Update_Load_Pds
         else
            call Update_Pds

      end /* End of condition */

      when ( Action == "UPDATE" & DSNtype == "SEQ" ) then do
         BUZMARG1 = TargDSN
         Address ISPEXEC 'GETMSG MSG(BUZD140) LONGMSG(BUZLNGER)'
         Say BUZLNGER

         call Update_Seq
      end /* End of condition */

      when ( Action == "DELETE" & DSNtype == "PDS" ) then do
         BUZMARG1 = TargDSN ; BUZMARG2 = "members from"
         Address ISPEXEC 'GETMSG MSG(BUZD167) LONGMSG(BUZLNGER)'
         Say BUZLNGER

         call Delete_Pds_Mem
      end /* End of condition */

      when ( Action == "DELETE" & DSNtype == "SEQ" ) then do
         BUZMARG1 = TargDSN ; BUZMARG2 = ""
         Address ISPEXEC 'GETMSG MSG(BUZD167) LONGMSG(BUZLNGER)'
         Say BUZLNGER

         call Delete_Seq
      end /* End of condition */

      otherwise do
        BUZMARG1 = offset
        BUZMARG2 = ActnDSN
        Address ISPEXEC 'GETMSG MSG('msgId.9') LONGMSG(BUZLNGER)'
        say BUZLNGER

        Call ExitProc(8)

      end /* End of OTHERWISE condition */
   end /* End of Select Statement */
end /* End of DO loop */

  call CleanUpWorkFiles(ActnDSN)

   say ""
   say "   ******   End of Deployment Process   ******   "
   say ""
   elapsed = Time('E')
   BUZMARG1 = elapsed
   Address ISPEXEC 'GETMSG MSG(BUZP110) LONGMSG(BUZLNGER)'
   Say BUZLNGER

exit /* End of Program */

/**********************************************************************
*********          FUNCTIONS START FROM HERE                ***********
**********************************************************************/
Load_Msg_Ids:

  drop msgId.

msgId.1  = "BUZD126" /* Message Id for LMINIT error             */
msgId.2  = "BUZD127" /* Message Id for LMOPEN error             */
msgId.3  = "BUZD131" /* Message Id for LMCLOSE error            */
msgId.4  = "BUZD132" /* Message Id for LMFREE error             */
msgId.5  = "BUZD166" /* Message Id for EXECIO error             */
msgId.6  = "BUZD136" /* Message Id for LMMDEL error             */
msgId.7  = "BUZD163" /* Message Id for LMCOPY error             */
msgId.8  = "BUZD164" /* Message Id for LMMSTATS error           */
msgId.9  = "BUZD168" /* Message Id for ActionDSN Format error   */
msgId.10 = "BUZD165" /* Message Id for ActionDSN not Found      */

return /* End of Function Load_Msg_Ids */

Delete_Seq:
   /* Delete requested SEQENTIAL file */
   x = OUTTRAP('DELSEQMSG.')
      Address TSO "DELETE '"TargDSN"'"
      Del_rc = rc
   x = OUTTRAP('OFF')

   parse upper var DELSEQMSG.1 IDC_Code IDC_Msg
   select
     when IDC_Code == "IDC0550I" then
        nop
     when IDC_Code == "IDC3012I" then do /* When dataset not found */
        BUZMARG1 = TargDSN
        Address ISPEXEC 'GETMSG MSG(BUZD169) LONGMSG(BUZLNGER)'
        Say BUZLNGER
     end
     otherwise do
        /* Display SYSTEM error messages before exiting the program */
        do z = 1 to DELSEQMSG.0
           say DELSEQMSG.z
        end
        call ExitProc(Del_rc)
     end
   end /* End of SELECT statement */

return /* End of Function Delete_Seq */

Delete_Pds_Mem:
  /* Deleting requested MEMBER from a PDS Data set.
     Sequence of actions performed in this function are :

                1) LMINIT
                2) LMOPEN
                3) LMMDEL
                4) LMCLOSE
                5) LMFREE
  */
   Address ISPEXEC "LMINIT DATAID(DOD)     " ||,
                   " DATASET('"TargDSN"')  " ||,
                                " ENQ(SHRW)"
   if rc <> 0 then call ErrorHandling(TargDSN rc errcode.1)

   Address ISPEXEC "LMOPEN DATAID(&DOD) OPTION(OUTPUT)"
   Open_rc = rc
   If (Open_rc <> 0) then do

      Address ISPEXEC "LMFREE DATAID(&DOD)"
      if rc <> 0 then call ErrorHandling(TargDSN rc msgId.4)

      call ErrorHandling(TargDSN Open_rc msgId.2)
   end

   /* Start reading Action records one by one from where its leftoff */
   do forever

      BUZMARG1 = PDSmem
      BUZMARG2 = TargDSN
      Address ISPEXEC 'GETMSG MSG(BUZD134) LONGMSG(BUZLNGER)'
      Say BUZLNGER

      Address ISPEXEC "LMMDEL DATAID(&DOD) MEMBER("PDSmem")"
      lmdel_RC = rc

      Select
        When (lmdel_RC == 0) Then
            Nop
        When (lmdel_RC == 8) Then
        Do
                    /* When member is not found, throw a Warning */
            BUZMARG1 = PDSmem
            Address ISPEXEC 'GETMSG MSG(BUZD135) LONGMSG(BUZLNGER)'
            Say BUZLNGER
        End
        otherwise do

          Address ISPEXEC "LMCLOSE DATAID(&DOD)"
          if rc <> 0 then call ErrorHandling(TargDSN rc msgId.3)

          Address ISPEXEC "LMFREE DATAID(&DOD)"
          if rc <> 0 then call ErrorHandling(TargDSN rc msgId.4)

          call ErrorHandling(PDSmem lmdel_RC msgId.6)
        end
      end /* End of SELECT statement */

      if ( offset == Actlist.0 ) then leave

      offset = offset + 1

      call check_format
      parse upper var Actlist.offset cDSNtype ";" cAction ";" cSrceDSN,
          ";" cTargDSN ";" cPDSmem ";" cDepType ";" cMDate ";" cMUser .

      if( DSNtype <> cDSNtype | Action <> cAction   | ,
          TargDSN <> cTargDSN | DepType <> cDepType ) then do

          offset = offset - 1 ; leave

      end

      DSNtype =  cDSNtype ; Action  =  cAction
      TargDSN =  cTargDSN ; DepType =  cDepType
      PDSmem  =  cPDSmem ; MDate= cMDate
      MUser   =  cMUser

   end /* End of DO Loop */

   Address ISPEXEC "LMCLOSE DATAID(&DOD)"
   if rc <> 0 then call ErrorHandling(TargDSN rc msgId.3)
   Address ISPEXEC "LMFREE DATAID(&DOD)"
   if rc <> 0 then call ErrorHandling(TargDSN rc msgId.4)

return /* End of Function Delete_Pds_Mem */

Update_Seq:

   /* IEBGENER to Update Data  */
   Address TSO "ALLOC F(SYSPRINT) NEW REUSE"
   Address TSO "ALLOC F(SYSUT1)  DA('"SrceDSN"') SHR REUSE"
   Address TSO "ALLOC F(SYSUT2) DA('"TargDSN"') SHR REUSE"
   Address TSO "ALLOC F(SYSIN) DUMMY REUSE "
   Address ISPEXEC "ISPEXEC SELECT PGM(IEBGENER)"
   Copy_rc = rc

   Address TSO  "EXECIO * DISKR SYSPRINT (FINIS STEM sysmsgs."

   x = Msg('off')
       Address TSO "FREE F(SYSIN,SYSPRINT,SYSUT1,SYSUT2)"
   x = Msg('on')

   If Copy_rc <> 0 Then do
      /* Display SYSTEM message from IEBGENER in case of failure */
      Do xmit = 1 to sysmsgs.0
          Say Strip(sysmsgs.xmit)
      End
      call ExitProc(Copy_rc)
   End

   /* Delete Source DSN After successful update */
   call CleanUpWorkFiles(SrceDSN)

return /* End of Function Update_Seq */

Update_Load_Pds:
 /* IEBCOPY to add/replace MEMBERS into LOAD/BINARY type PDS */
   Address TSO "ALLOC F(SYSPRINT) NEW REUSE"
   Address TSO "ALLOC F(INDD)  DA('"SrceDSN"') SHR REUSE"
   Address TSO "ALLOC F(OUTDD) DA('"TargDSN"') SHR REUSE"
   Address TSO "ALLOC F(SYSIN) NEW REUSE"

   Address TSO "ALLOC F(SYSUT4) NEW" ||,
                   " CYLINDERS UNIT(SYSALLDA) SPACE(5 10)"

 /**********************************************************
    From zOS 2.1 onwards we can use the COPYGROUP for alias
    or if data set is a PDSE we can use COPYGRP for alias
 ***********************************************************/
   DROP SYSLINE.

   SYSLINE.1 = " COPYGROUP OUTDD=OUTDD,INDD=((INDD,R))"
   Address TSO "EXECIO * DISKW SYSIN (STEM SYSLINE. FINIS)"
   Address ISPEXEC "ISPEXEC SELECT PGM(IEBCOPY)"
   Copy_rc = rc
   Address TSO "EXECIO * DISKR SYSPRINT (STEM sysinfo. FINIS)"

   x = Msg('off')
       Address TSO "FREE F(SYSIN,SYSUT4,OUTDD,INDD,SYSPRINT)"
   x = Msg('on')

   If Copy_rc <> 0 Then
   Do
      /* Display SYSTEM message from IEBCOPY in case of failure */
      Do xmit = 1 to sysinfo.0
          Say Strip(sysinfo.xmit)
      End
      call ExitProc(Copy_rc)
   End
   /* Display output details from IEBCOPY */
   Do xmit = 1 to sysinfo.0
     parse var sysinfo.xmit sysw1 sysw2 sysw3 .
     if ( strip(sysw2) == "MEMBER" ) then
        Say "   ***   MEMBER = " left(sysw3,8) "  ***   "
   End

   /* Delete Source DSN After successful update */
   call CleanUpWorkFiles(SrceDSN)

return /* End of Function Update_Load_PDS */

Update_Pds:
/* Add (or) Replace requested MEMBERS into a PDS
   Sequence of actions performed in this function are :

           1) LMINIT for Input
           2) LMMSTATS (only if Metadata is present)
           3) LMFREE for Input
*/
   Address ISPEXEC "LMINIT DATAID(UDID) "  ||,
                   " DATASET('"SrceDSN"')" ||,
                                    " ENQ(SHRW)"
   if rc <> 0 then call ErrorHandling(SrceDSN rc msgId.1)

        Address ISPEXEC "CONTROL ERRORS RETURN    "

   do forever
    /*  say "   ---   MEMBER = " left(PDSmem,8) "  ---   "  */

        /* Updating Metadata - User, Modified Date and Time */
        If  MUser <> "" then
        do
          parse var MDate ModDate ModTime
          Address ISPEXEC "LMMSTATS DATAID(&UDID) MEMBER("PDSmem")" ||,
                     "USER("MUser") " ||,
                 "MODDATE4("ModDate") MODTIME("ModTime") "  ||,
                 "CREATED4("ModDate") "
          Stat_rc = rc
          If (Stat_rc <> 0) then do
             Address ISPEXEC "LMFREE DATAID(&UDID)"
             if rc <> 0 then call ErrorHandling(SrceDSN rc msgId.4)

             call ErrorHandling(PDSmem Stat_rc msgId.8)
          end
      end /* End of Metadata Update check */

      if offset == Actlist.0 then leave

      offset = offset + 1
      call check_format
      parse upper var Actlist.offset cDSNtype ";" cAction ";" cSrceDSN ,
           ";" cTargDSN ";" cPDSmem ";" cDepType ";" cMDate ";" cMUser .

      if ( SrceDSN <> cSrceDSN | Action <> cAction ) then do

          offset = offset - 1 ; leave

      end

      DSNtype =  cDSNtype ; Action  =  cAction
      TargDSN =  cTargDSN ; DepType =  cDepType
      PDSmem  =  cPDSmem ; MDate = cMDate
      MUser   =  cMUser ; SrceDSN = cSrceDSN

   end /* End of DO Loop */

         Address ISPEXEC "CONTROL ERRORS CANCEL"

   Address ISPEXEC "LMFREE DATAID(&UDID)"
   if rc <> 0 then call ErrorHandling(SrceDSN rc msgId.4)

   /* Call IEBCOPY proc for a one shot copy from temp to target */
   call Update_Load_Pds

return /* End of Function Update_PDS */

check_format:
  /* If no of semicolons(;) in the Action record are not 7 then error */
   p =  Actlist.offset
   q = ';'
   occrs = length(space(p,0)) - length(space(translate(p,' ',q),0))

   if occrs <> 7 then do
        BUZMARG1 = offset
        BUZMARG2 = ActnDSN
        Address ISPEXEC 'GETMSG MSG('msgId.9') LONGMSG(BUZLNGER)'
        say BUZLNGER
        Call ExitProc(8)
   end

return /* End of function check_format */

ErrorHandling:
parse arg BUZMARG1 BUZMARG2 messageId
    /* Exit program with an Error Message */
    Address ISPEXEC 'GETMSG MSG('messageId') LONGMSG(BUZLNGER)'
    say BUZLNGER
    Call ExitProc(BUZMARG2)

return /* End of ErrorHandling Rountine */

CleanUpWorkFiles:
parse arg fileName
   x = Msg('off')
      Address TSO " DELETE '"fileName"' "
   x = Msg('on')
return /* End of function CleanUpWorkFiles */

ExitProc :
   /* Exiting program with passed Return Code */
   Parse arg exit_rc

   ZISPFRC = exit_rc
   Address ISPEXEC "VPUT (ZISPFRC) SHARED"

Exit exit_rc /* End of ExitProc */

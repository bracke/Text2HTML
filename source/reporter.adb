--------------------------------------------------------------------------------
--                                                                            --
-- Copyright (C) 2004, RISC OS Ada Library (RASCAL) developers.               --
--                                                                            --
-- This library is free software; you can redistribute it and/or              --
-- modify it under the terms of the GNU Lesser General Public                 --
-- License as published by the Free Software Foundation; either               --
-- version 2.1 of the License, or (at your option) any later version.         --
--                                                                            --
-- This library is distributed in the hope that it will be useful,            --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU           --
-- Lesser General Public License for more details.                            --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public           --
-- License along with this library; if not, write to the Free Software        --
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA    --
--                                                                            --
--------------------------------------------------------------------------------

-- $Author$
-- $Date$
-- $Revision$

-- Reporter bindings for Ada 95
-- Reporter module by Chris Morison & Martin Avison
-- Ada 95 bindings by Stefan Bellon

with Kernel;                        use Kernel;
with Interfaces.C;                  use Interfaces.C;
with System;
with System.Storage_Elements;
with OS;

package body Reporter is

   OS_CLI           : constant Interfaces.C.Unsigned := 16#5#;
   Report_Clear     : constant Interfaces.C.Unsigned := 16#54C8B#;
   Report_Close     : constant Interfaces.C.Unsigned := 16#54C8D#;
   Report_On        : constant Interfaces.C.Unsigned := 16#54C8E#;
   Report_Off       : constant Interfaces.C.Unsigned := 16#54C8F#;
   Report_Quit      : constant Interfaces.C.Unsigned := 16#54C8A#;
   Report_Where     : constant Interfaces.C.Unsigned := 16#54C84#;
   Report_Text0     : constant Interfaces.C.Unsigned := 16#54C80#;
   Report_TextS     : constant Interfaces.C.Unsigned := 16#54C81#;
   Report_Poll      : constant Interfaces.C.Unsigned := 16#54C85#;
   Report_Registers : constant Interfaces.C.Unsigned := 16#54C83#;
   Report_Dump      : constant Interfaces.C.Unsigned := 16#54C86#;

   Error : oserror_access;
   Regs  : aliased Kernel.SWI_Regs;

   function Address_To_C_Int (Addr : in System.Address)
                              return Interfaces.C.Int is
   begin
      return Interfaces.C.Int (System.Storage_Elements.To_Integer (Addr));
   end Address_To_C_Int;

   procedure Call_OS_CLI (Command : in String) is
   
      Local_String : String := Command & Character'Val (0);

   begin
      Regs.R(0) := Address_To_C_Int (Local_String'Address);
      Error := Kernel.SWI (OS_CLI, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Call_OS_Cli: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;
   end Call_OS_CLI;

   procedure Report (Text : in String) is
   begin
      Call_OS_CLI ("Report " & Text);
   end Report;

   procedure Clear (Text : in String := "") is
   begin
      Call_OS_CLI ("ReportClear " & Text);
   end Clear;

   procedure On (Text : in String := "") is
   begin
      Call_OS_CLI ("ReportOn " & Text);
   end On;

   procedure Off (Text : in String := "") is
   begin
      Call_OS_CLI ("ReportOff " & Text);
   end Off;

   procedure Close is
   begin
      Error := Kernel.SWI (Report_Close, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Close: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;

   end Close;

   procedure Quit is
   begin
      Error := Kernel.SWI (Report_Quit, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Quit: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;

   end Quit;

   procedure Where is
   begin
      Error := Kernel.SWI (Report_Where, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Where: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;
   end Where;

   procedure Text (Text : in String := "") is
      Local_String : String := Text & Character'Val (0);
   begin
      Regs.R(0) := Address_To_C_Int (Local_String'Address);
      Error := Kernel.SWI (Report_Text0, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Text: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;
   end Text;

   procedure Poll (Reason : in Natural := 0) is
   begin
      Regs.R(0) := Interfaces.C.int (Reason);
      Error := Kernel.SWI (Report_Poll, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Poll: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;
   end Poll;

   procedure Registers (Text : in String := "") is
      Local_String : String := Text & Character'Val (0);
   begin
      Regs.R(0) := Address_To_C_Int (Local_String'Address);
      Regs.R(1) := 0;
      Regs.R(2) := 0;
      Error := Kernel.SWI (Report_Registers, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Registers: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;
   end Registers;

   procedure Dump (Start : in System.Address;
                   Length: in Natural;
                   Width : in Positive) is
   begin
      Regs.R(0) := Address_To_C_Int (Start);
      Regs.R(1) := Int(Length);
      Regs.R(2) := Int(Width);
      Error := Kernel.SWI (Report_Dump, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Reporter.Dump: " & To_Ada(Error.ErrMess)));
	     OS.Raise_Error(Error);
      end if;
   end Dump;

   procedure Save (Text : in String := "") is
   begin
      Call_OS_CLI ("ReportSave " & Text);
   end Save;

end Reporter;

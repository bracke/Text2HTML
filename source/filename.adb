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

-- StrongEd$WrapWidth=256
-- StrongEd$Mode=Ada
--

with Interfaces.C;                     use Interfaces.C;
with System.Storage_Elements;          use System.Storage_Elements;
with Kernel;                           use Kernel;
with OS;                               use OS;
with Utility;                          use Utility;
with Memory;                           use Memory;

with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Unbounded;

with Ada.Characters.Handling;
with Reporter;

package body FileName is

   use Ada.Strings.Unbounded;

   OS_FSControl : constant Interfaces.C.unsigned := 16#29#;

   -- Converts names with wildcards into path
   function Convert_Path (Path : in String) return String is

      Register        : aliased Kernel.swi_regs;
      Error           : oserror_access;
      Buffer_Size     : integer;
      Null_Path       : string := Path & ASCII.NUL;
   begin
      -- Get needed buffer size
      Register.R(0) := 37;
      Register.R(1) := Adr_To_Int(Null_Path'Address);
      Register.R(2) := 0;
      Register.R(4) := 0;
      Register.R(5) := 0;

      Error := Kernel.swi ( OS_FSControl, register'Access, register'Access );
      Buffer_Size := (- Integer(Register.R(5))) + 1;
      if Error /= Null then
         return Path;
      end if;

      declare
         Buffer : String(1..Buffer_Size);
      begin
         -- Convert path
         Register.R(0) := 37;
         Register.R(1) := Adr_To_Int(Null_Path'Address);
         Register.R(2) := Adr_To_Int(Buffer'Address);
         Register.R(4) := 0;
         Register.R(5) := int(Buffer_Size);
         
         Error := Kernel.swi ( OS_FSControl, register'Access, register'Access);
         if Error /= Null then
            return Path;
         end if;
         return MemoryToString(Buffer'Address);
      end;

   end Convert_Path;

   --

   function Get_FS (Path      : in string;
                    Separator : in string := ":")
   	                                         return string is
   begin
      return Head(Path,Index(Convert_Path(Path),Separator,forward)-1);
   end Get_FS;

   --

   function Get_FS_And_Drive (Path      : in string;
                              Separator : in string := "$")
   	                                         return string is
   begin
      return Head(Path,Index(Convert_Path(Path),Separator,forward));
   end Get_FS_And_Drive;

   --   

   function Get_Leaf_Name (Path     : in String;
                           Separator : in String := ".") return String is

      Path2 : String := Convert_Path(Path);
   begin
      return Tail(Path2,
                  Path2'Length-(Index(Path2,Separator,backward)));
   end Get_Leaf_Name;
   
   --
      
   function Get_Path (Path      : in String;
                     Separator : in String := ".") return String is

      Path2 : String := Convert_Path(Path);
   begin
      if Path2'Length = 0 then
         return "";
      end if;
      if Count(Path2,Separator) = 0 then
         return "";
      end if;
     return Delete(Path2,Index(Path2,Separator,backward),Path2'Last);
   end Get_Path;

   --

   function Web_To_RISCOS (Path : in String) return String is

      index : integer := 0;
      Text  : Unbounded_String := U(Path);
   begin
      index := Ada.Strings.Unbounded.Index(Text,"..");
      while index > 0 loop
         Ada.Strings.Unbounded.Replace_Element(Text,index,'^');
         Ada.Strings.Unbounded.Delete(Text,index+1,index+1);
         index := Ada.Strings.Unbounded.Index(Text,"..");
      end loop;
      Ada.Strings.Unbounded.Trim(Text,Both);
      declare
         Str : String := S(Text);
      begin
         for i in Str'Range loop
            case Str(i) is
            when '.'    => Str(i) := '/';
            when '/'    => Str(i) := '.';
            when others => null;
            end case;
         end loop;
         return Str;
      end;
   end Web_TO_RISCOS;

   --

   function RISCOS_To_web (Path : in String) return String is

      index : integer := 0;
      Text  : Unbounded_String;
      Str   : String := Path;
   begin
      for i in Str'Range loop
         case Str(i) is
         when '/'    => Str(i) := '.';
         when '.'    => Str(i) := '/';
         when others => null;
         end case;
      end loop;
      Text  := U(Str);
      index := Ada.Strings.Unbounded.Index(Text,"^");
      while index > 0 loop
         Ada.Strings.Unbounded.Insert(Text,index,"..");
         Ada.Strings.Unbounded.Delete(Text,index,index+1);
         index := Ada.Strings.Unbounded.Index(Text,"^");
      end loop;
      Ada.Strings.Unbounded.Trim(Text,Both);
      return S(Text);
   end RISCOS_To_Web;

   --

   function To_RISCOS (Original          : in String;
                       Remove_Extensions : in Boolean := true;
                       Lower_Case_Tail   : in Boolean := true) return String is

      Index : integer;
      Name  : ustring := U(Original);
   begin
      if Remove_Extensions then
         Index := Ada.Strings.Unbounded.Index(Name,"/",backward);
         while Index > 1 loop
            Name := Ada.Strings.Unbounded.Head(Name,Index-1);
            Index := Ada.Strings.Unbounded.Index(Name,"/",backward);
         end loop;
      end if;
      declare
         Result : string := S(Name);
      begin
         if Lower_Case_Tail then
            Result := Ada.Characters.Handling.To_Lower(Result);
            Result(Result'First) := Ada.Characters.Handling.To_Upper(Result(Result'First));
         end if;
         return Result;
      end;
   end To_RISCOS;

   --

   function App_To_Dir (Original : in String) return String is

      Name : ustring := U(Original);
   begin
      while Ada.Strings.Unbounded.Index(Name,"!",forward) = 1 loop
         Name := Ada.Strings.Unbounded.Tail(Name,Ada.Strings.Unbounded.Length(Name)-1);
      end loop;
      return S(Name);
   end App_To_Dir;

   --

   function Seperate_Paths (Path : in String) return Paths_Type is

      Nr          : Natural            := Count(Path,",") + 1;
      Paths       : Paths_Type(1..Nr);
      Path_String : Unbounded_String   := U(Path);
      Len         : Natural            := Length(Path_String);
      i           : Natural            := 0;
      Paths_Index : Positive           := 1;
   begin
      while Len /= 0 loop
         I := Index(Path_String,",");
         if I > 0 then
            Paths(Paths_Index) := U(Slice(Path_String,1,I-1));
            Path_String := U(Slice(Path_String,I+1,Len));
         else
            Paths(Paths_Index) := Path_String;
            Path_STring := U("");
         end if;
         Paths_Index := Paths_Index + 1;
         Len  := Length(Path_String);
      end loop;
      return Paths;
   end Seperate_Paths;

   --
   
end FileName;

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

--
-- @filename FileInternal.ads
-- @author Bent Bracke
-- @date 20.12.2003
-- @version 1.0
-- @brief FileInternal types and methods.
-- 
-- History: 
-- 

with Utility;                    use Utility;
with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;
with Ada.Finalization;

package FileInternal is

   subtype Real_FileHandle_Type is System.Unsigned_Types.Unsigned;
   
   type UString_Ptr is access UString;
   type File_Access_Type is (Read,Write,ReadWrite);

   type FileHandle_Type(File        : UString_Ptr;
                        File_Access : File_Access_Type) is new Ada.Finalization.Limited_Controlled with private;

   type rotimetyp is range 0..16#FFFFFFFFFF#;
   type roattributestyp is new Integer;   
   
   file_error_message : Ustring;
   file_error_number  : Integer;
   File_Error         : exception;

   --
   -- Close file.
   --
   procedure Close (File : in FileHandle_Type);

   --
   -- Goto end of file.
   --
   procedure GotoEnd (File : in FileHandle_Type);

   --
   -- Returns true if end of file has been reached.
   --
   function IsEOF (File : in FileHandle_Type) return boolean;

   --
   -- Returns the size of the file.
   --
   function Get_Size (File : in FileHandle_Type) return natural;

   --
   -- Goto start of file.
   --
   procedure Goto_Start (File : in FileHandle_Type);

   --
   -- Returns file index.
   --
   function Get_Ptr (File : in FileHandle_Type) return Integer;

   --
   -- Sets file index to 'ptr'
   --
   procedure Set_Ptr (File : in FileHandle_Type;
                      Ptr  : in integer);

   --
   -- Read a single byte from file.
   --
   function Get_Byte (File : in FileHandle_Type) return Integer;

   --
   -- Writes a single byte to file.
   --
   procedure Put_Byte (File : in FileHandle_Type;
                       Byte : in Integer);

   --
   -- Move a Nr of bytes forward in file.
   --
   procedure Skip_Bytes (File : in FileHandle_Type;
                         Nr   : in Integer);

   --
   -- Writes a string (Line) to file.
   --
   procedure Put_String (File      : in FileHandle_Type;
                         Line      : in String;
                         Attach_LF : in Boolean := true);

   --
   -- Returns the number of lines in file.
   --
   function Get_Lines (File : in FileHandle_Type;
                       Trim : in boolean          := true) return integer;                                                                

   --
   -- Reads a line from file.
   --
   function Read_Line (File : in FileHandle_Type) return String;

   --
   -- Ensures that the size of the 'File' is not less than 'Size'. Note that the extent is not changed. 
   --
   procedure Ensure_Size (File : in FileHandle_Type;
                          Size : in Integer);

   --
   -- Loads file into 'Buffer'.
   --
   procedure Load_File (FileName : in String;
                        Buffer   : in Address);

   --
   -- Saves 'Buffer' to file.
   --
   procedure Save_File (FileName  : in String;
                        Buffer    : in Address;
                        Bufferend : in Address;
                        FileType  : in Integer);

   --
   -- Reads a number (Length) of bytes from file.
   --
   procedure Get_Bytes (File   : in FileHandle_Type;
                        Buffer : in Address;
                        Length : in Integer);

   --
   -- Writes a number (Length) of bytes to files.
   --
   procedure Put_Bytes (File   : in FileHandle_Type;
                        Buffer : in Address;
                        Length : in Integer);

   --
   -- Returns internal filehandle.
   --
   function Get_Real_FileHandle (File : in FileHandle_Type) return Real_FileHandle_Type;

                      
private

   procedure Initialize (The : in out FileHandle_Type);
   
   procedure Finalize (The : in out FileHandle_Type);

   type FileHandle_Type(File        : UString_Ptr;
                        File_Access : File_Access_Type) is
                                      new Ada.Finalization.Limited_Controlled with
   record
     Path         : UString              := File.all;
     Access_Type  : File_Access_Type     := File_Access;
     Handle       : Real_FileHandle_Type := -1;
   end record;
   
end FileInternal;

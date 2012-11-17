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
-- @filename FileName.ads
-- @author Bent Bracke
-- @date 20.12.2003
-- @version 1.0
-- @brief FileName related types and methods.
-- 
-- History: 
-- 
with Utility; use Utility;

package FileName is

   type Paths_Type is array (natural range <>) of UString;
   type Path_Pointer is access Paths_Type;

   --
   -- This function canonicalises the 'Path'.
   --It takes a filename (which may contain wildcards) and converts it into a complete pathmame, including disc and directory name when not provided.
   --
   --#tab
   --{/}Example{/}:	{Fcode}fred{f} may convert to {Fcode}SCSI::Gamma.$.Utils.Fred{f}
   --#tab
   --
   function Convert_Path (Path : in String) return String;

   --
   -- Returns the filing system part the 'Path'.
   --
   function Get_FS (Path      : in String;
                    Separator : in String := ":")
   	                                         return String;

   --
   -- Returns the filing system and drive part the 'Path'.
   --
   function Get_FS_And_Drive (Path      : in String;
                              Separator : in String := "$")
   	                                         return String;

   --
   -- Takes a full pathname and returns the leafname.
   --
   function Get_Leaf_Name (Path      : in String;
                           Separator : in String := ".") return String;
            

   --
   -- takes a full pathname and returns the path (not including the seperator).
   --
   function Get_Path (Path     : in String;
                     Separator : in String := ".") return String;
            

   --
   -- Converts a web-format path to RISC OS format.
   --
   function Web_To_RISCOS (Path : in String) return String;

   --
   -- Converts a RISC OS format path to web format.
   --
   function RISCOS_To_web (Path : in String) return String;

   --
   -- Convets a DOS/Windows format path to RISC OS format.
   --
   function To_RISCOS (Original          : in String;
                       Remove_Extensions : in Boolean := true;
                       Lower_Case_Tail   : in Boolean := true) return String;

   --
   -- Remove plings from beginning of name.
   --
   function App_To_Dir (Original : in String) return String;

   --
   -- Splits a paths string containing several commaseperated paths into its elements and returns an array of those paths.
   --
   function Seperate_Paths (Path : in String) return Paths_Type;

end FileName;

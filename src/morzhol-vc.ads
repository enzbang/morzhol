------------------------------------------------------------------------------
--                                Morzhol                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                      Pascal Obry - Olivier Ramonat                       --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package Morzhol.VC is

   use Ada.Strings.Unbounded;

   type VCS is interface;

   type Commit_Log is tagged record
      Revision : Unbounded_String;
      Date     : Unbounded_String;
      Author   : Unbounded_String;
      Message  : Unbounded_String;
   end record;

   type Log is array (Positive range <>) of Commit_Log;

   function Commit
     (Engine   : in VCS;
      Filename : in String;
      Message  : in String;
      Author   : in String := "")
     return Boolean is abstract;
   --  Checking in the changes. Message is the commit log message
   --  If no author is given use the default one a the system

   function Lock
     (Engine : in VCS; Filename : in String) return Boolean is abstract;
   --  Lock file

   function Add
     (Engine   : in VCS;
      Filename : in String;
      Author   : in String := "")
      return Boolean is abstract;
   --  Add a new file or do the Initial check-in
   --  If no author is given use the default one a the system

   function Remove
     (Engine : in VCS; Filename : in String) return Boolean is abstract;
   --  Remove a file

   function Get_Log
     (Engine   : in VCS;
      Filename : in String;
      Limit    : in Natural := 0)
     return Log is abstract;
   --  Returns the log of the given file. Limit = 0 means no limit.

   function Diff
     (Engine       : in VCS;
      Filename     : in String;
      From_Version : in String;
      To_Version   : in String)
     return String is abstract;
   --  Returns the diff

   function Image (VCS_Log : in Log) return String;
   --  Returns the image of a log

end Morzhol.VC;

--
--  Navigate
--  Todo lists are hieracial.

with Ada.Containers.Vectors;

with Database.Jobs;

package Navigate is

   procedure Top;
   --  Go to top level.

   procedure Parent;
   --  Go one level up. If top level: stay there.

   function Path_Image return String;
   --  Image of path.

--   type Level_Index is new Positive;
--   package Path_Vectors is
--      new Ada.Containers.Vectors (Level_Index,
--                                  Database.Job_Id);
--   Path : Path_Vectors.Vector;

   procedure Build_Path (Job : in Database.Jobs.Job_Id);
   --  Build Path from Job and up.

   function Current_Job return Database.Jobs.Job_Id;
   --  Get current job.

   procedure Refresh_List;
   --  Refresh job naviagtion list.

   procedure Lookup_Job (Text    : in     String;
                         Job     :    out Database.Jobs.Job_Id;
                         Success :    out Boolean);
   --  Loopup Text as job in navigation list. Job is set on Success.


   subtype Ref_Type is String (1 .. 3);
   type Ref_Pair is
      record
         Ref   : Ref_Type;
         Job   : Database.Jobs.Job_Id;
         Level : Natural;  --  0: Top level
      end record;

   package Ref_Vectors is
      new Ada.Containers.Vectors (Database.Jobs.Job_Index, Ref_Pair);

   type List_Type is
      record
         Set     : Database.Jobs.Job_Sets.Vector;
         Refs    : Ref_Vectors.Vector;
         Current : Database.Jobs.Job_Id;
      end record;

   List : List_Type;


end Navigate;

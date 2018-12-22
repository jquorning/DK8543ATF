--
--  Database
--

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Interfaces;
with SQLite;

package Database is

   package US renames Ada.Strings.Unbounded;

   type Job_Id is new Interfaces.Integer_64;

   subtype Job_Ref is String (1 .. 3);

   type Job_Desc is
      record
         Ref   : String (1 .. 3);
         Id    : Job_Id;
         Title : US.Unbounded_String;
      end record;

   package Job_Vectors is new Ada.Containers.Vectors (Positive, Job_Desc);
   type Job_Set is
      record
         Vector  : Job_Vectors.Vector;
      end record;

   function Get_Current_Job return Job_Id;
   --  Get current job from persistent database storage.

   procedure Set_Current_Job (Job : in Job_Id);
   --  Set current jon in persistent storage.

   Cur_Jobs : Job_Set;  --  Current jobs
   Top_Jobs : Job_Set;  --  Top level jobs

   procedure Open;

   Top_Level : constant Job_Id := 0;
   All_Jobs  : constant Job_Id := -1;
   procedure Get_Jobs (Jobs   :    out Job_Set;
                       Parent : in     Job_Id);

   type Job_Info is
      record
         Title  : US.Unbounded_String;
         Parent : Job_Id;
         Owner  : US.Unbounded_String;
      end record;

   function Get_Job_Info (Job : in Job_Id) return Job_Info;


   type Job_Event is
      record
         Stamp : US.Unbounded_String;
         Kind  : US.Unbounded_String;
      end record;

   package Event_Lists is
      new Ada.Containers.Vectors (Positive, Job_Event);

   function Get_Job_Events (Job : in Job_Id)
                           return Event_Lists.Vector;


   procedure Add_Job (Id     : in Job_Id;
                      Title  : in String;
                      Parent : in Job_Id;
                      Owner  : in String);

   function Get_Job_Id return Job_Id;

   procedure Lookup_Job (Text    : in     String;
                         Job     :    out Job_Id;
                         Success :    out Boolean);
   --  Loopup Text as job in top level. Job is set on Success,

   procedure Transfer (Job       : in Job_Id;
                       To_Parent : in Job_Id);

private

   DB : SQLite.Data_Base;

end Database;

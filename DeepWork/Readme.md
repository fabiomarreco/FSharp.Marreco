Requirements: 
=============

Work can be either
- Deep
- Shallow


Slots are periods of same size (say 30min)

Slots can be either
- Assigned to Work (deep, shallow)
- Reserved to Offwork Commitment

an Offwork commitment may be lunch, diner, sleep, play with kids, commute, etc.
The same Work can be assigned to multiple time slots


DeepWork can only be assigned to a minimum amount of slots (at least 2 slots (1h))
The same slot can be assigned to multiple shallow Work
The slot assigned to a deep task cannot assigned to anything else

Schedule is the act of assigning a slot to something

//------------------------
differences between day and week schedules


```plantuml
(*) -> "Schedule Work"
-> "Work Assigned"
if "Has conflict?" then 
    -> [No] (*)
else 
    --> [Yes] "Unsubscribe Work"

--> "Work Unsubscribed"
--> "Add Pending Assignement"
--> "Pending Assignement Included"
```


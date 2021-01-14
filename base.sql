select 
	date(yo.appointment_dateTime)
	,count(yo.id) as quantity
	,yo.operationType
	,yo.linerCode 
	,ct.nombre as c_type
	,if(yo.operationType like 'IMPO', c2.cSize, yo.requiredContainer_size) as c_size
from yard_operation yo
left join container c2 on c2.id = yo.containerId
inner join contenedores_tipos ct on ct.codigo = if(yo.operationType like 'IMPO', c2.cType, yo.requiredContainer_type)
where 
	date(yo.appointment_dateTime) > '2020-01-01'
	#and yo.operationType like 'IMPO'
group by 
	date(yo.appointment_dateTime)
	,operationType 
	#,linerCode 
	,c_type 
	,c_size
;
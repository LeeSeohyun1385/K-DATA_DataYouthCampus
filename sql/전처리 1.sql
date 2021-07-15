select *
from d_base3_2;


--기대도수 구하기 

select a.시군
     , a.지정구분
     , nvl(b.관측도수, 0) 관측도수  --널값처리nvl
     , b.관측도수
     , a.기대도수
from (select 시군
           , 지정구분
           , 기대도수_시군 * 기대도수_지정구분 / 전체도수 기대도수
      from (select 시군
            , count(*) 기대도수_시군
       from d_base3_2
       group by 시군) x, ---기대도수_시군
      (select 지정구분
            , count(*) 기대도수_지정구분 
       from d_base3_2 
       group by 지정구분) y, ---기대도수_지정구분
      (select count(*) 전체도수
       from d_base3_2) z  ---전체도수
      ) a, ---기대도수 집합
      (select 시군
            , 지정구분
            , count(*) 관측도수
       from d_base3_2
       group by 시군, 지정구분) b  ---관측도수 집합
where a.시군 = b.시군(+)
  and a.지정구분 = b.지정구분(+) ;
    
--이너조인(그냥하면)
--레프트아웃조인(+)


select a.시군
     , a.지정구분
     , nvl(b.관측도수, 0) 관측도수
     , b.관측도수
     , a.기대도수
from (select 시군
           , 지정구분
           , 기대도수_시군 * 기대도수_지정구분 / 전체도수 기대도수
      from (select 시군
            , count(*) 기대도수_시군
       from d_base3_2
       group by 시군) x, 
      (select 지정구분
            , count(*) 기대도수_지정구분 
       from d_base3_2 
       group by 지정구분) y,
      (select count(*) 전체도수
       from d_base3_2) z  
      ) a, 
      (select 시군
            , 지정구분
            , count(*) 관측도수
       from d_base3_2
       group by 시군, 지정구분) b 
where a.시군 = b.시군(+)
  and a.지정구분 = b.지정구분(+) ;
select *
from d_base3_2;


--��뵵�� ���ϱ� 

select a.�ñ�
     , a.��������
     , nvl(b.��������, 0) ��������  --�ΰ�ó��nvl
     , b.��������
     , a.��뵵��
from (select �ñ�
           , ��������
           , ��뵵��_�ñ� * ��뵵��_�������� / ��ü���� ��뵵��
      from (select �ñ�
            , count(*) ��뵵��_�ñ�
       from d_base3_2
       group by �ñ�) x, ---��뵵��_�ñ�
      (select ��������
            , count(*) ��뵵��_�������� 
       from d_base3_2 
       group by ��������) y, ---��뵵��_��������
      (select count(*) ��ü����
       from d_base3_2) z  ---��ü����
      ) a, ---��뵵�� ����
      (select �ñ�
            , ��������
            , count(*) ��������
       from d_base3_2
       group by �ñ�, ��������) b  ---�������� ����
where a.�ñ� = b.�ñ�(+)
  and a.�������� = b.��������(+) ;
    
--�̳�����(�׳��ϸ�)
--����Ʈ�ƿ�����(+)


select a.�ñ�
     , a.��������
     , nvl(b.��������, 0) ��������
     , b.��������
     , a.��뵵��
from (select �ñ�
           , ��������
           , ��뵵��_�ñ� * ��뵵��_�������� / ��ü���� ��뵵��
      from (select �ñ�
            , count(*) ��뵵��_�ñ�
       from d_base3_2
       group by �ñ�) x, 
      (select ��������
            , count(*) ��뵵��_�������� 
       from d_base3_2 
       group by ��������) y,
      (select count(*) ��ü����
       from d_base3_2) z  
      ) a, 
      (select �ñ�
            , ��������
            , count(*) ��������
       from d_base3_2
       group by �ñ�, ��������) b 
where a.�ñ� = b.�ñ�(+)
  and a.�������� = b.��������(+) ;
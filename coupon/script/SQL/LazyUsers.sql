/****** Script for SelectTopNRows command from SSMS  ******/
--Coupon purchased without being viewed
SELECT DISTINCT
      [CouponId]
	  ,[UserId]
  FROM [coupon].[dbo].[CouponDetailTrain]
  EXCEPT
SELECT DISTINCT
      [VIEW_COUPON_ID_hash]
	  ,[USER_ID_Hash]
  FROM [coupon].[dbo].[CouponVisitTrain]

--Users who never viewed
SELECT DISTINCT
      [USER_ID]
  FROM [coupon].[dbo].[UserList]
  EXCEPT
SELECT DISTINCT
      [USER_ID_Hash]
  FROM [coupon].[dbo].[CouponVisitTrain]

--Users who never purchased
SELECT DISTINCT
      [USER_ID]
  FROM [coupon].[dbo].[UserList]
  EXCEPT
SELECT DISTINCT
      [UserId]
  FROM [coupon].[dbo].[CouponDetailTrain]

--Users who never viewed nor purchased any coupon
SELECT DISTINCT
      [USER_ID]
  FROM [coupon].[dbo].[UserList]
  EXCEPT
SELECT DISTINCT
      [USER_ID_Hash]
  FROM [coupon].[dbo].[CouponVisitTrain]
  EXCEPT
SELECT DISTINCT
      [UserId]
  FROM [coupon].[dbo].[CouponDetailTrain]
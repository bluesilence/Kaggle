/****** Script for SelectTopNRows command from SSMS  ******/
--Select count of eligible coupons by Ken
SELECT 
	  COUNT([CouponId]) AS CouponIdCount
      ,[KenEN]
  FROM [coupon].[dbo].[CouponListTest]
  GROUP BY
	[KenEN]

--Select count of eligible users by Ken
SELECT 
	  COUNT([USER_ID]) AS UserIdCount
      ,[PREF_NAME_EN]
  FROM [coupon].[dbo].[EligibleUserList]
  GROUP BY
	[PREF_NAME_EN]

--Infer Ken of user without PREF in UserList from Ken of coupons purchased by the user
--Not applicable, since there are many users who purchased coupons in more than 1 Ken
SELECT
	[UserId]
	,COUNT(DISTINCT [KenEN]) AS KenCount
FROM
(SELECT
	[UserId]
	,[KenEN]
	,COUNT(couponKen.[CouponId]) AS CouponIdCount
FROM
(
	SELECT
		[CouponId]
		,[UserId]
	FROM
	(
		SELECT
			[USER_ID]
		FROM [coupon].[dbo].[EligibleUserList]
		WHERE PREF_NAME_EN IS NULL
	) users
	LEFT OUTER JOIN
	(
		SELECT DISTINCT
			[UserId]
			,[CouponId]
		FROM [coupon].[dbo].[CouponDetailTrain]
	) purchasedCoupons
	ON users.[USER_ID] = purchasedCoupons.[UserId]
) couponUsers
INNER JOIN [coupon].[dbo].[CouponListTrain] couponKen
ON couponUsers.[CouponId] = couponKen.[CouponId]
GROUP BY
	[UserId]
	,[KenEN]
) result
GROUP BY [UserId]
HAVING COUNT(DISTINCT [KenEN]) > 1

/****** Script for SelectTopNRows command from SSMS  ******/
USE [coupon]
GO

--Q: Test coupons also in training set?
--A: No.
SELECT
	a.[CouponId]
FROM
(
SELECT DISTINCT
	[CouponId]
FROM [dbo].[CouponListTest]
) a
INNER JOIN
(
SELECT DISTINCT
	[CouponId]
FROM [dbo].[CouponListTrain]
) b
ON a.[CouponId] = b.[CouponId]

--Select Most popular coupons within the same sex & age range for users without PREF info
SELECT
	CouponId
	,[SEX]
	,[AgeBin]
	,SUM(CouponCount) AS PurchasedCount
FROM
(
	SELECT
		[USER_ID]
		,[SEX]
		,FLOOR([AGE] / 5) AS [AgeBin]
	FROM [dbo].[EligibleUserList]
	WHERE PREF_NAME IS NULL
  ) a
  INNER JOIN
  (
	SELECT [UserId], CouponId, SUM([ITEM_COUNT]) AS CouponCount
	FROM [dbo].[CouponDetailTrain]
	GROUP BY [UserId], CouponId
  ) b
  ON a.[USER_ID] = b.[UserId]
  GROUP BY
	CouponId
	,SEX
	,AgeBin
  ORDER BY PurchasedCount DESC

  --Dig the purchase pattern of 1 coupon in Tokyo
  --Select the most popular coupon in Tokyo
  SELECT 
		[dbo].[CouponDetailTrain].CouponId AS CouponId
		,SUM([ITEM_COUNT]) AS CouponCount
	FROM [dbo].[CouponDetailTrain]
	INNER JOIN [dbo].[CouponListTrain]
	ON [dbo].[CouponDetailTrain].[SmallAreaEN] = [dbo].[CouponListTrain].[SmallAreaEN]
	WHERE KenEN = 'Tokyo'
	GROUP BY [dbo].[CouponDetailTrain].CouponId
	ORDER BY CouponCount DESC

  --The most purchased coupon in Tokyo: a262c7ff56a5cd3de3c5c40443f3018c
 SELECT
	[PurchaseMonth]
	,[SEX]
	,[AGE]
	,SUM([ITEM_COUNT]) AS PurchasedCouponCount
	,COUNT([USER_ID]) AS PurchasedUserCount
FROM
  (
  SELECT
	UserId
	,CAST(CAST(DATEPART(year, [PurchaseDate]) AS VARCHAR(4)) + RIGHT('0' + CAST(DATEPART(month, [PurchaseDate]) AS VARCHAR(2)), 2) + '01' AS DATETIME) AS [PurchaseMonth]
	,ITEM_COUNT
  FROM [dbo].[CouponDetailTrain]
  WHERE CouponId = 'a262c7ff56a5cd3de3c5c40443f3018c'
  ) a
  INNER JOIN
  (
  SELECT DISTINCT
	[USER_ID]
	,[SEX]
	,[AGE]
  FROM [dbo].[UserList]
  WHERE PREF_NAME_EN = 'Tokyo'
  ) b
  ON a.UserId = b.[USER_ID]
  GROUP BY
	[PurchaseMonth]
	,[SEX]
	,[AGE]

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

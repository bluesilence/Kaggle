/****** Script for SelectTopNRows command from SSMS  ******/
--Stats about coupons' DispPeriod
SELECT
	DispPeriod
	,COUNT(1) AS DispPeriodCount
FROM 
(
	SELECT
	DATEPART(day, DispEnd - DispFrom) AS DispPeriod
	FROM [coupon].[dbo].[CouponListTrain]
) a
GROUP BY
DispPeriod
ORDER BY DispPeriodCount DESC

--How many coupons are purchased after 1 week of the coupon's DispFrom?
SELECT
	PurchaseDelay
	,SUM(ITEM_COUNT) AS PurchasedCount
FROM
(
	SELECT
		b.CouponId
		,ITEM_COUNT
		,DATEPART(day, PurchaseDate - DispFrom) AS PurchaseDelay
	FROM
	(
		SELECT
			CouponId
			,DispFrom
			,DispPeriod
		FROM 
		(
			SELECT
				CouponId
				,DispFrom
				,DATEPART(day, DispEnd - DispFrom) AS DispPeriod
			FROM [coupon].[dbo].[CouponListTrain]
		) a
		WHERE DispPeriod > 7
	) b
	INNER JOIN [coupon].[dbo].[CouponDetailTrain] c
	ON b.CouponId = c.CouponId
) d
WHERE PurchaseDelay >= 7
GROUP BY PurchaseDelay
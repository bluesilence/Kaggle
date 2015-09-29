/****** Script for SelectTopNRows command from SSMS  ******/
SELECT COUNT([USER_ID]) AS USER_COUNT
	   ,CONVERT(date, [REG_DATE]) AS [REG_DATE]
	   ,CONVERT(date, [WITHDRAW_DATE]) AS [WITHDRAW_DATE]
	   ,[SEX]
	   ,[AGE]
	   ,[PREF_NAME_EN]
	   ,[LAT]
	   ,[LOG]
	   /*,(CASE WHEN [WITHDRAW_DATE] IS NOT NULL 
			THEN DATEDIFF(day, [REG_DATE], [WITHDRAW_DATE])
			ELSE DATEDIFF(day, [REG_DATE], GETUTCDATE())
		END) AS [EXIST_DAYS]
	   ,(CASE WHEN [WITHDRAW_DATE] IS NOT NULL 
			THEN 1
			ELSE 0
		END) AS [WITHDRAWN]*/
  FROM [coupon].[dbo].[UserList] userList
  LEFT OUTER JOIN [coupon].[dbo].[PrefectureLocations] locations
  ON userList.[PREF_NAME_EN] = locations.[EN_PREF]
  GROUP BY
  [REG_DATE]
  ,[WITHDRAW_DATE]
  ,[SEX]
  ,[AGE]
  ,[PREF_NAME_EN]
  ,[LAT]
  ,[LOG]
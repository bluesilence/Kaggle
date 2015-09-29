USE [coupon]
GO

/****** Object:  Table [dbo].[EligibleUserList]    Script Date: 9/29/2015 10:16:16 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[EligibleUserList](
	[PREF_NAME] [varchar](64) NULL,
	[REG_DATE] [datetime] NULL,
	[SEX] [nchar](1) NULL,
	[AGE] [numeric](4, 0) NULL,
	[WITHDRAW_DATE] [datetime] NULL,
	[USER_ID] [varchar](32) NOT NULL,
	[PREF_NAME_EN] [varchar](64) NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO



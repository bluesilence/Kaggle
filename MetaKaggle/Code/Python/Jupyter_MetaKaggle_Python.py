import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
plt.style.use('bmh')
%matplotlib inline

import sqlite3

con = sqlite3.connect('D:/Kaggle/MetaKaggle/database.sqlite')

top_ranked_users = pd.read_sql_query("""
SELECT *
FROM Users
WHERE Ranking != 'NaN'
ORDER BY Ranking ASC
LIMIT 1000
""", con)

top_ranked_users

users_submissions = pd.read_sql_query("""
SELECT
    submissions.SubmittedUserId AS UserId
    ,users.DisplayName AS UserDisplayName
    ,users.Ranking AS UserRanking
    ,submissions.DateSubmitted AS DateSubmitted
FROM
(
    SELECT SubmittedUserId, DateSubmitted
    FROM Submissions
    WHERE IsAfterDeadline = 0
    AND ScoreStatus = 1
    AND PrivateScore != 'NaN'
    AND PublicScore != 'NaN'
    AND SubmittedUserId IN
    (
        SELECT Id
        FROM Users
        WHERE Ranking != 'NaN'
    )
) submissions
INNER JOIN Users users
ON users.Id = submissions.SubmittedUserId
ORDER BY
    DateSubmitted ASC
""", con)

users_submissions["DateSubmitted"] = pd.to_datetime(users_submissions["DateSubmitted"], errors = 'raise')
users_submissions["DateSubmitted"] = users_submissions["DateSubmitted"].dt.date
users_submissions["DateSubmitted"]

users_submissions.to_csv('D:/Kaggle/MetaKaggle/Data/Output/users_submissions.tsv', sep='\t', encoding='utf-8')                              
users_submissions


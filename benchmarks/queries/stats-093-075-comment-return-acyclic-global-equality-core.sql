SELECT COUNT(*) AS witness_count
FROM comments AS c,
     posts AS p,
     posthistory AS ph,
     votes AS v,
     badges AS b,
     users AS u,
     postlinks AS pl2,
     comments AS c2,
     votes AS v2,
     users AS u2
WHERE b.userid = p.owneruserid
  AND b.userid = u.id
  AND c.postid = p.id
  AND c.postid = ph.postid
  AND c.postid = pl2.postid
  AND c.postid = v.postid
  AND c2.postid = pl2.relatedpostid
  AND c2.postid = v2.postid
  AND u2.id = v2.userid;

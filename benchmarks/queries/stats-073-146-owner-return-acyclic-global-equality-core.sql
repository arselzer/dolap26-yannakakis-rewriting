SELECT COUNT(*) AS witness_count
FROM comments AS c,
     posts AS p,
     postlinks AS pl,
     posthistory AS ph,
     votes AS v,
     users AS u,
     posts AS p2,
     comments AS c3,
     users AS u3
WHERE c.postid = p.id
  AND c.postid = ph.postid
  AND c.postid = pl.postid
  AND c.postid = v.postid
  AND c3.postid = p2.id
  AND c3.postid = pl.relatedpostid
  AND c3.userid = u3.id
  AND p.owneruserid = u.id;


SELECT
  g.ref,
  gn.name,
  MAX(ghv.version) AS max_version,
  gb.brief
FROM
  gitrepo AS g
INNER JOIN
  gitreponame AS gn ON g.ref = gn.ref
INNER JOIN
  gitrepoheadversion AS ghv ON gn.hash = ghv.hash
LEFT JOIN
  gitrepobrief AS gb ON g.ref = gb.ref AND ghv.hash = gb.hash
GROUP BY
  g.ref, gn.name;



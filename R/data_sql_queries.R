q_df_general_with_project_fields <- r"(
    SELECT
        p.project_id project_id,
        p.name project_name,
        p.description project_description,
        r.name researcher_name,
        rg.name group_name,
        COUNT(*) OVER (PARTITION BY p.project_id, rg.name) sort_digit
    FROM projects p
        LEFT JOIN research_participation rp
            ON (p.project_id = rp.project_id)
        LEFT JOIN researchers r
            ON (rp.researcher_id = r.employee_id)
        LEFT JOIN research_groups rg
            ON (r.main_research_group = rg.group_id)
    --WHERE
        --p.project_id = 4001
    ORDER BY p.project_id, sort_digit DESC, rg.name
)"

q_df_general <- r"(
CREATE OR REPLACE VIEW df_researchers_and_companies_they_worked_with AS
WITH df_researchers_and_companies_they_worked_with AS (
 	SELECT DISTINCT
      r.name researcher_name,
      rg.name researcher_group,
      p.project_id project_id,
      p.name project_name,
      p.description project_description,
      rp.days_allocated,
      c.name company_name
  FROM projects p
      LEFT JOIN research_participation rp
          ON (p.project_id = rp.project_id)
      LEFT JOIN researchers r
          ON (rp.researcher_id = r.employee_id)
      LEFT JOIN research_groups rg
          ON (r.main_research_group = rg.group_id)
      LEFT JOIN project_board pb
          ON (p.project_id = pb.project_id)
      LEFT JOIN company_contacts cc
          ON (pb.contact_id = cc.contact_id)
      LEFT JOIN companies c
          ON (cc.company_id = c.company_id)
)
SELECT
	*,
	CAST(COUNT(*) OVER (PARTITION BY researcher_name, company_name) AS int) sort_digit,
	1 AS sum_digit
FROM df_researchers_and_companies_they_worked_with;
SELECT * FROM df_researchers_and_companies_they_worked_with;
)"

q_df_for_project_details_stacked_bar_chart <- r"(
-- DROP VIEW df_researchers_and_companies_they_worked_with;
CREATE OR REPLACE VIEW df_researchers_and_companies_they_worked_with AS
WITH df_researchers_and_companies_they_worked_with AS (
 	SELECT DISTINCT
 	  r.employee_id researcher_id,
      r.name researcher_name,
      rg.name researcher_group,
      p.project_id project_id,
      p.name project_name,
      p.description project_description,
      rp.days_allocated,
      c.name company_name
  FROM projects p
      LEFT JOIN research_participation rp
          ON (p.project_id = rp.project_id)
      LEFT JOIN researchers r
          ON (rp.researcher_id = r.employee_id)
      LEFT JOIN research_groups rg
          ON (r.main_research_group = rg.group_id)
      LEFT JOIN project_board pb
          ON (p.project_id = pb.project_id)
      LEFT JOIN company_contacts cc
          ON (pb.contact_id = cc.contact_id)
      LEFT JOIN companies c
          ON (cc.company_id = c.company_id)
)
SELECT
	*,
	CAST(COUNT(*) OVER (PARTITION BY researcher_name, company_name) AS int) sort_digit,
	1 AS sum_digit
FROM df_researchers_and_companies_they_worked_with;



CREATE OR REPLACE VIEW df_projects_and_fields AS
	WITH df_projects_and_all_fields AS (
		SELECT
		    p.project_id project_id,
		    p.name project_name,
		    p.description project_description,
		    r.name researcher_name,
		    rg.name group_name,
		    COUNT(*) OVER (PARTITION BY p.project_id, rg.name) sort_digit
		FROM projects p
		    LEFT JOIN research_participation rp
		        ON (p.project_id = rp.project_id)
		    LEFT JOIN researchers r
		        ON (rp.researcher_id = r.employee_id)
		    LEFT JOIN research_groups rg
		        ON (r.main_research_group = rg.group_id)
		--WHERE
		--    p.project_id = 4001
		ORDER BY p.project_id, sort_digit DESC, rg.name
	)
	SELECT
		(
			SELECT paf.group_name
			FROM df_projects_and_all_fields paf
			WHERE paf.project_id = p.project_id
			ORDER BY paf.sort_digit DESC
			LIMIT 1
		) AS project_field,
		*
	FROM projects p;

--SELECT * FROM df_researchers_and_companies_they_worked_with;
--SELECT * FROM df_projects_and_fields;


CREATE OR REPLACE VIEW df_for_project_details_stacked_bar_chart AS
SELECT
	project_id,
	project_name,
	company_name,
	project_field,
	researcher_id,
	researcher_name,
	sort_digit,
	sum_digit
FROM df_researchers_and_companies_they_worked_with rac
	INNER JOIN df_projects_and_fields paf
		USING (project_id);

SELECT * FROM df_for_project_details_stacked_bar_chart;
)"





# bq1-a1 
q_df_for_project_graph_network <- r"(
CREATE OR REPLACE VIEW df_for_project_graph_network AS
WITH researcher_links AS (
    SELECT
        r.employee_id AS researcher_id,
        r.name AS researcher_name,
        p.project_id,
        p.name AS project_name,
        p.type AS project_type
    FROM research_participation rp
    JOIN researchers r ON rp.researcher_id = r.employee_id
    JOIN projects p ON rp.project_id = p.project_id
),
company_links AS (
    SELECT
        c.company_id,
        c.name AS company_name,
        p.project_id,
        p.name AS project_name,
        p.type AS project_type
    FROM project_board pb
    JOIN company_contacts cc ON pb.contact_id = cc.contact_id
    JOIN companies c ON cc.company_id = c.company_id
    JOIN projects p ON pb.project_id = p.project_id
)
SELECT
    rl.project_id,
    rl.project_name,
    rl.project_type,
    rl.researcher_id,
    rl.researcher_name,
    cl.company_id,
    cl.company_name
FROM researcher_links rl
LEFT JOIN company_links cl USING (project_id);
SELECT * FROM df_for_project_graph_network;
)"
-- Table: Research_Groups
CREATE TABLE Research_Groups (
    group_id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL
);

-- Table: Researchers
CREATE TABLE Researchers (
    employee_id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    main_research_group INT,
    FOREIGN KEY (main_research_group) REFERENCES Research_Groups(group_id)
);

-- Table: Projects
CREATE TABLE Projects (
    project_id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    responsible_group INT,
    responsible_employee INT,
    total_budget DECIMAL(15,2),
    funding_source ENUM('PWO','TETRA','Other Government','CONTRACT') NOT NULL,
    type ENUM('Research','Services','Training','Talk') NOT NULL,
    start_date DATE,
    end_date DATE,
    FOREIGN KEY (responsible_group) REFERENCES Research_Groups(group_id),
    FOREIGN KEY (responsible_employee) REFERENCES Researchers(employee_id)
);

-- Table: Companies
CREATE TABLE Companies (
    company_id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    sectors VARCHAR(255),
    description TEXT,
    size ENUM('Small','Medium','Large')
);

-- Table: Company_Contacts
CREATE TABLE Company_Contacts (
    contact_id INT PRIMARY KEY,
    company_id INT,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255),
    phone VARCHAR(50),
    job_title VARCHAR(50),
    department VARCHAR(50),
    FOREIGN KEY (company_id) REFERENCES Companies(company_id)
);

-- Table: Project_Board (relations between companies via contacts and their role in project)
CREATE TABLE Project_Board (
    project_id INT,
    contact_id INT,
    role ENUM('Participation','Steering Committee','Funding') NOT NULL,
    PRIMARY KEY (project_id, contact_id),
    FOREIGN KEY (project_id) REFERENCES Projects(project_id),
    FOREIGN KEY (contact_id) REFERENCES Company_Contacts(contact_id)
);

-- Table: Research_Participation
CREATE TABLE Research_Participation (
    project_id INT,
    researcher_id INT,
    days_allocated INT NOT NULL,
    role ENUM('Lead','worker','viewer') NOT NULL,
    PRIMARY KEY (project_id, researcher_id),
    FOREIGN KEY (project_id) REFERENCES Projects(project_id),
    FOREIGN KEY (researcher_id) REFERENCES Researchers(employee_id)
);

-- I want the shortest path between two employees or an employee which is looking to contact a company in a certain sector,...
-- What can you get out of the project description? (LLM/RAG/PGVector?)


package com.molkky.molkky.ihm.InfosPageDisplayTest;


import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class InfosPageDisplayTest {

    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private CourtRepository courtRepository;
    @Autowired
    private UserTounamentRoleRepository userTournamentRoleRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;
    private String emailStaff = "connexion.staff@gmail.com";
    private String passwordStaff = "staff123";

    private String teamName = "TeamTestStaff1";
    private String teamName2 = "TeamTestStaff2";


    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
        if(tournamentRepository.findByName("TournamentTestStaff")==null) {
            Tournament tournament = new Tournament();
            tournament.setName("TournamentTestStaff");
            tournament.setVisible(true);
            tournamentRepository.save(tournament);
            Tournament tournament2 = new Tournament();
            tournament2.setName("tournamentTestStaff2");
            tournament2.setVisible(true);
            tournamentRepository.save(tournament2);
            Team team = new Team();
            team.setName(teamName);
            teamRepository.save(team);
            Team team2 = new Team();
            team.setName(teamName2);
            teamRepository.save(team2);
            Round round = new Round();
            round.setTournament(tournament);
            roundRepository.save(round);
            Court court = new Court();
            court.setName("courtTestStaff");
            courtRepository.save(court);
            Match match = new Match();
            match.setRound(round);
            match.setScoreTeam1(25);
            match.setScoreTeam2(50);
            match.setCourt(court);
            match.setFinished(true);
            List<Team> teams = new ArrayList<Team>();
            teams.add(team);
            teams.add(team2);
            match.setTeams(teams);
            if(userRepository.findUserByEmail(emailStaff)==null) {
                User staff = new User();
                staff.setEmail(emailStaff);
                staff.setPassword(passwordStaff);
                userRepository.save(staff);
                UserTounamentRole userTournamentRoleStaff = new UserTounamentRole();
                userTournamentRoleStaff.setRole(UserRole.STAFF);
                userTournamentRoleStaff.setUser(staff);
                userTournamentRoleStaff.setTournament(tournament);
                userTournamentRoleRepository.save(userTournamentRoleStaff);
            }
        }
    }

    @Test
    void testInfos(){
        config.getDriver().get(url + "/infos");
        Assertions.assertEquals("Information", config.getDriver().getTitle());

    }
}



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
    private UserTournamentRoleRepository userTournamentRoleRepository;
    @Autowired
    private MatchRepository matchRepository;
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
            /*Tournament tournament = new Tournament();
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
            team2.setName(teamName2);
            teamRepository.save(team2);
            Round round = new Round();
            round.setTournament(tournament);
            roundRepository.save(round);
            Court court = new Court();
            court.setName("courtTestStaff");
            courtRepository.save(court);
            if(userRepository.findUserByEmail(emailStaff)==null) {
                User staff = new User();
                staff.setEmail(emailStaff);
                staff.setPassword(passwordStaff);
                userRepository.save(staff);
                UserTournamentRole userTournamentRoleStaff = new UserTournamentRole();
                userTournamentRoleStaff.setRole(UserRole.STAFF);
                userTournamentRoleStaff.setUser(staff);
                userTournamentRoleStaff.setTournament(tournament);
                userTournamentRoleRepository.save(userTournamentRoleStaff);
            }
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
            User staff = userRepository.findUserByEmail(emailStaff);
            UserTournamentRole userTournamentRoleStaff2 = new UserTournamentRole();
            userTournamentRoleStaff2.setRole(UserRole.STAFF);
            userTournamentRoleStaff2.setUser(staff);
            userTournamentRoleStaff2.setTournament(tournament);
            userTournamentRoleRepository.save(userTournamentRoleStaff2);
            UserTournamentRole userTournamentRoleStaff3 = new UserTournamentRole();
            userTournamentRoleStaff3.setRole(UserRole.STAFF);
            userTournamentRoleStaff3.setUser(staff);
            userTournamentRoleStaff3.setTournament(tournament2);
            userTournamentRoleRepository.save(userTournamentRoleStaff3);
            match.setStaff(staff);
            matchRepository.save(match);

             */
        }
    }

    @Test
    void testInfos(){
        config.getDriver().get(url + "/infos");
        Assertions.assertEquals("Informations personnelles", config.getDriver().getTitle());
    }
}



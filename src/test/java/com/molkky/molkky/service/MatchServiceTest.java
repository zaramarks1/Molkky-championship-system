package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import type.SetTeamIndex;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@SpringBootTest
class MatchServiceTest {
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private MatchService matchService;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Test
    void createMatchModelsTest() {
//        given
        List<Match> matches = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            matches.add(matchRepository.save(new Match()));
        }
//        when
        List<MatchModel> matchModels = MatchService.createMatchModels(matches);
//        then
        Assertions.assertEquals(10, matchModels.size());
        for (int i = 0; i < 10; i++) {
            Assertions.assertEquals(matches.get(i).getId(), matchModels.get(i).getId());
            Assertions.assertEquals(matches.get(i).getFinished(), matchModels.get(i).getFinished());
            Assertions.assertEquals(matches.get(i).getNbSets(), matchModels.get(i).getNbSets());
        }
    }

    @Test
    void createMatchModelTest() {
//        given
        Match match = matchRepository.save(new Match());
//        when
        MatchModel matchModel = MatchService.getMatchModelFromEntity(match);
//        then
        Assertions.assertEquals(match.getId(), matchModel.getId());
        Assertions.assertEquals(match.getFinished(), matchModel.getFinished());
        Assertions.assertEquals(match.getNbSets(), matchModel.getNbSets());
    }

    @Test
    void getMatchFromModelTest() {
//        given
        Match match = matchRepository.save(new Match());
        MatchModel matchModel = MatchService.getMatchModelFromEntity(match);
//        when
        Match match2 = matchService.getMatchFromModel(matchModel);
//        then
        Assertions.assertEquals(match.getId(), match2.getId());
        Assertions.assertEquals(match.getFinished(), match2.getFinished());
        Assertions.assertEquals(match.getNbSets(), match2.getNbSets());
    }

    @Test
    void getUserIndexTestAndIsInMatch1() {
//        given
        Match match = matchRepository.save(new Match());
        Team team1 = teamRepository.save(new Team());
        Team team2 = teamRepository.save(new Team());

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = userRepository.save(new User());
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRoleRepository.save(userTournamentRole1);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        match.setTeams(Arrays.asList(team1, team2));
        match = matchRepository.save(match);
//        when
        SetTeamIndex index = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user1));
//        then
        Assertions.assertEquals(SetTeamIndex.TEAM1, index);
        Assertions.assertTrue(matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user1)));
    }

    @Test
    void getUserIndexTestAndIsInMatch2() {
//        given
        Match match = matchRepository.save(new Match());
        Team team1 = teamRepository.save(new Team());
        Team team2 = teamRepository.save(new Team());

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = userRepository.save(new User());
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRoleRepository.save(userTournamentRole1);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        match.setTeams(Arrays.asList(team2, team1));
        match = matchRepository.save(match);
//        when
        SetTeamIndex index = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user1));
//        then
        Assertions.assertEquals(SetTeamIndex.TEAM2, index);
        Assertions.assertTrue(matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user1)));
    }

    @Test
    void getUserIndexTestAndIsInMatch3() {
//        given
        Match match = matchRepository.save(new Match());
        Team team1 = teamRepository.save(new Team());
        Team team2 = teamRepository.save(new Team());

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = userRepository.save(new User());
        User user2 = userRepository.save(new User());
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRoleRepository.save(userTournamentRole1);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        match.setTeams(Arrays.asList(team2, team1));
        match = matchRepository.save(match);
//        when
        SetTeamIndex index = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user2));
//        then
        Assertions.assertEquals(SetTeamIndex.ORGA, index);
        Assertions.assertFalse(matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user2)));
    }

}

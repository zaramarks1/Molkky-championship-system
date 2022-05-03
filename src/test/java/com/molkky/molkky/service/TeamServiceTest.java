package com.molkky.molkky.service;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTounamentRole;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTounamentRoleRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
 class TeamServiceTest {

        @Autowired
        private TeamService teamService;

        @Mock
        private CreateTeamModel teamModel;

        @Mock
        private AddPlayerlistModel addPlayerlistModel;

        @Mock
        private AddPlayerModel addPlayerModel1;

        @Autowired
        private TeamRepository teamRepository;

        @Autowired
        private  TournamentRepository tournamentRepository;

        @Autowired
        private UserTounamentRoleRepository userTounamentRoleRepository;

        @Autowired
        private UserRepository userRepository;


        @Test
         void testCreateTeam(){
            Tournament tournament = new Tournament();
            tournamentRepository.save(tournament);

            String teamName = "TeamMock"+Math.floor((Math.random() * 1000));
            Mockito.when(teamModel.getTournament()).thenReturn(tournament.getId());
            Mockito.when(teamModel.getName()).thenReturn(teamName);

            Team team = teamService.create(teamModel);

            Assertions.assertNotNull(team);
            Assertions.assertEquals(teamName,team.getName());
            Assertions.assertEquals(tournament.getId(),team.getTournament().getId());

        }

        @Test
         void testAddPlayerNonExist(){
            Tournament tournament = new Tournament();
            tournamentRepository.save(tournament);

            Team team = new Team();
            team.setTournament(tournament);
            teamRepository.save(team);

            String email = "prenom"+Math.floor((Math.random() * 1000))+"@gmail.com";
            User user = new User();
            user.setEmail(email);

            List<AddPlayerModel> player = new ArrayList<>();
            player.add(addPlayerModel1);

            Mockito.when(addPlayerlistModel.getPlayers()).thenReturn(player);
            Mockito.when(addPlayerModel1.getTeamId()).thenReturn(team.getId());
            Mockito.when(addPlayerModel1.addPlayer()).thenReturn(user);

            teamService.addPlayers(addPlayerlistModel);

            UserTounamentRole userTounamentRole = userTounamentRoleRepository.findByTeamAndTournament(team,tournament);
            User userBDD = userRepository.findUserByEmail(email);

            Assertions.assertNotNull(userBDD);
            Assertions.assertNotNull(userTounamentRole);
            Assertions.assertEquals(userTounamentRole.getUser().getId(),userBDD.getId());
            Assertions.assertEquals(userTounamentRole.getUser().getEmail(),email);
        }

    @Test
     void testAddPlayerExist(){
        Tournament tournament = new Tournament();
        tournamentRepository.save(tournament);

        Team team = new Team();
        team.setTournament(tournament);
        teamRepository.save(team);

        String email = "prenom"+Math.floor((Math.random() * 1000))+"@gmail.com";
        User user = new User();
        user.setEmail(email);
        userRepository.save(user);

        List<AddPlayerModel> player = new ArrayList<>();
        player.add(addPlayerModel1);

        Mockito.when(addPlayerlistModel.getPlayers()).thenReturn(player);
        Mockito.when(addPlayerModel1.getTeamId()).thenReturn(team.getId());
        Mockito.when(addPlayerModel1.addPlayer()).thenReturn(user);

        teamService.addPlayers(addPlayerlistModel);

        UserTounamentRole userTounamentRole = userTounamentRoleRepository.findByTeamAndTournament(team,tournament);

        Assertions.assertNotNull(userTounamentRole);
        Assertions.assertEquals(userTounamentRole.getUser().getId(),user.getId());
        Assertions.assertEquals(userTounamentRole.getUser().getEmail(),email);
    }

        @Test
         void testAreAllDistinctUsers(){
            List<User> users = new ArrayList<>();

            User user1 = new User();
            user1.setEmail("aurelien.masson@outlook.com");
            users.add(user1);
            Assertions.assertTrue(teamService.areAllDistinct(users));

            User user2 = new User();
            user2.setEmail("zara.marks@outlook.com");
            users.add(user2);
            Assertions.assertTrue(teamService.areAllDistinct(users));

            User user3 = new User();
            user3.setEmail("zara.marks@outlook.com");
            users.add(user3);
            Assertions.assertFalse(teamService.areAllDistinct(users));
        }

        @Test
         void testCreateCodeLength(){
            String code = teamService.createCode(10);
            Assertions.assertEquals(10,code.length());
        }
}

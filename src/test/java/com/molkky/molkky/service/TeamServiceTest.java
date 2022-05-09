package com.molkky.molkky.service;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.ArrayList;
import java.util.List;

@RunWith(MockitoJUnitRunner.class)
public class TeamServiceTest {

        @InjectMocks
        private TeamService teamService;

        @Mock
        private CreateTeamModel teamModel;

        @Mock
        private  TournamentRepository tournamentRepository;

        @Mock
        private TeamRepository teamRepository;

        @Mock
        private AddPlayerlistModel addPlayerlistModel;

        @Mock
        private AddPlayerModel addPlayerModel1;

        @Mock
        private UserRepository userRepository;

        @Mock
        private UserTournamentRoleRepository userTounamentRoleRepository;

        @Mock
        private User user;

        @BeforeEach
        public void setUp(){
            MockitoAnnotations.openMocks(this);
        }

        @Test
        public void testCreateTeam(){
            Integer idTournament = 1;
            String teamName = "TeamMock"+Math.floor(Math.random() * 100);
            Tournament tournament = new Tournament();
            tournament.setId(idTournament);

            Mockito.when(teamModel.getTournament()).thenReturn(1);
            Mockito.when(tournamentRepository.findById(idTournament)).thenReturn(tournament);
            Mockito.when(teamModel.getName()).thenReturn(teamName);
            Mockito.when(teamRepository.save(Mockito.any(Team.class))).thenAnswer(i -> i.getArguments()[0]);

            Team team = teamService.create(teamModel);

            Mockito.verify(teamModel,Mockito.times(1)).getTournament();
            Mockito.verify(teamModel,Mockito.times(1)).getName();
            Mockito.verify(tournamentRepository,Mockito.times(1)).findById(idTournament);
            Mockito.verify(teamRepository,Mockito.times(1)).save(team);
            Mockito.verifyNoMoreInteractions(teamModel);
            Mockito.verifyNoMoreInteractions(teamModel);
            Mockito.verifyNoMoreInteractions(tournamentRepository);
            Mockito.verifyNoMoreInteractions(teamRepository);

            Assertions.assertNotNull(team,"Team null");
            Assertions.assertEquals(teamName,team.getName(),"Nom different");
            Assertions.assertEquals(idTournament,team.getTournament().getId(),"id different");
        }

        @Test
        public void testAddPlayerNonExist(){
            List<AddPlayerModel> listPlayer = new ArrayList<>();
            listPlayer.add(addPlayerModel1);

            Tournament tournament = new Tournament();
            tournamentRepository.save(tournament);


            String surname = "Surname"+Math.floor(Math.random() * 100);
            String forename = "Forename"+Math.floor(Math.random() * 100);
            String email = surname+"."+forename+"@test.fr";

            Integer id_team = 1;
            Team teamMock = new Team();
            teamMock.setId(id_team);

            Mockito.when(addPlayerlistModel.getPlayers()).thenReturn(listPlayer);
            Mockito.when(addPlayerModel1.getTeamId()).thenReturn(id_team);
            Mockito.when(teamRepository.findById(listPlayer.get(0).getTeamId())).thenReturn(teamMock);

            Mockito.when(addPlayerModel1.addPlayer()).thenReturn(user);
            Mockito.when(user.getEmail()).thenReturn(email);

            Mockito.when(userRepository.existsUserByEmail(email)).thenReturn(false);
            Mockito.when(userRepository.save(Mockito.any(User.class))).thenAnswer(i -> i.getArguments()[0]);

            teamService.addPlayers(addPlayerlistModel);

            Mockito.verify(addPlayerlistModel,Mockito.times(1)).getPlayers();
            Mockito.verify(teamRepository,Mockito.times(1)).findById(listPlayer.get(0).getTeamId());
            Mockito.verify(addPlayerModel1,Mockito.times(3)).getTeamId();
            Mockito.verify(addPlayerModel1,Mockito.times(1)).addPlayer();
            Mockito.verify(user,Mockito.atMost(1)).getEmail();
            Mockito.verify(user,Mockito.atMost(1)).setPassword(Mockito.anyString());
            Mockito.verify(userRepository,Mockito.atMost(1)).existsUserByEmail(email);
            Mockito.verify(userRepository,Mockito.atMost(1)).save(Mockito.any(User.class));
            Mockito.verify(userTounamentRoleRepository,Mockito.times(1)).saveAll(Mockito.<UserTournamentRole>anyList());

            Mockito.verifyNoMoreInteractions(addPlayerlistModel);
            Mockito.verifyNoMoreInteractions(addPlayerModel1);
            Mockito.verifyNoMoreInteractions(teamRepository);
            Mockito.verifyNoMoreInteractions(user);
            Mockito.verifyNoMoreInteractions(userRepository);
            Mockito.verifyNoMoreInteractions(userTounamentRoleRepository);
        }

        @Test
        public void testAddPlayerExist(){
            List<AddPlayerModel> listPlayer = new ArrayList<>();
            listPlayer.add(addPlayerModel1);

            Tournament tournament = new Tournament();
            tournamentRepository.save(tournament);

            String surname = "Surname"+Math.floor(Math.random() * 100);
            String forename = "Forename"+Math.floor(Math.random() * 100);
            String email = surname+"."+forename+"@test.fr";

            Integer id_team = 1;
            Team teamMock = new Team();
            teamMock.setId(id_team);

            Mockito.when(addPlayerlistModel.getPlayers()).thenReturn(listPlayer);
            Mockito.when(addPlayerModel1.getTeamId()).thenReturn(id_team);
            Mockito.when(teamRepository.findById(listPlayer.get(0).getTeamId())).thenReturn(teamMock);

            Mockito.when(addPlayerModel1.addPlayer()).thenReturn(user);
            Mockito.when(user.getEmail()).thenReturn(email);

            Mockito.when(userRepository.existsUserByEmail(email)).thenReturn(true);

            teamService.addPlayers(addPlayerlistModel);

            Mockito.verify(user,Mockito.times(2)).getEmail();
            Mockito.verify(userRepository,Mockito.times(1)).existsUserByEmail(email);
            Mockito.verify(userRepository, Mockito.times(1)).findUserByEmail(email);

            Mockito.verifyNoMoreInteractions(user);
            Mockito.verifyNoMoreInteractions(userRepository);
        }

        @Test
        public void testAreAllDistinctUsers(){
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
        public void testCreateCodeLength(){
            String code = teamService.createCode(10);
            Assertions.assertEquals(10,code.length());
        }
}

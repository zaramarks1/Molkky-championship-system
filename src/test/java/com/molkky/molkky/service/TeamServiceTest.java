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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

@WebMvcTest(value = TeamService.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class TeamServiceTest {

    @Autowired
    private TeamService teamService;

    @MockBean
    private CreateTeamModel teamModel;

    @MockBean
    private  TournamentRepository tournamentRepository;

    @MockBean
    private TeamRepository teamRepository;

    @MockBean
    private AddPlayerlistModel addPlayerlistModel;

    @MockBean
    private AddPlayerModel addPlayerModel1;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private UserTournamentRoleRepository userTounamentRoleRepository;

    @MockBean
    private User user;

    @Test
    void testCreateTeam(){
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
    void testAddPlayerNonExist(){
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
    void testAddPlayerExist(){
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

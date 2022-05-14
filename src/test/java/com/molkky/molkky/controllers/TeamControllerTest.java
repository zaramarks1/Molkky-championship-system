package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.EmailSenderService;
import com.molkky.molkky.service.TeamService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = TeamController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class TeamControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private EmailSenderService emailSenderService;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private TeamRepository teamRepository;

    @Mock
    private AddPlayerlistModel addPlayerlistModel;

    @Mock
    private AddPlayerModel addPlayerModel1;

    @Mock
    private AddPlayerModel addPlayerModel2;

    @MockBean
    private TeamService teamService;

    @Test
    void testTeamGetMethod() throws Exception{
        this.mockMvc.perform(get("/team/create/"))
                .andDo(print())
                .andExpect(model().attributeExists("tournaments"))
                .andExpect(model().attributeExists("team"))
                .andExpect(view().name("/team/create"));
    }

    @Test
    void testPostTeamMethod() throws Exception{
        Tournament tournament = new Tournament();
        tournament.setNbPlayersPerTeam(2);

        Team team = new Team();
        team.setTournament(tournament);

        Mockito.when(teamService.create(Mockito.any(CreateTeamModel.class))).thenReturn(team);

        mockMvc.perform(post("/team/create")
                        .param("nom","Test")
                        .param("tournamentId","1")
                        .flashAttr("teamModel",new CreateTeamModel()))
                .andDo(print())
                .andExpect(view().name("/team/addPlayer"))
                .andExpect(model().attribute("team",team))
                .andExpect(model().attributeExists("teamModel"))
                .andExpect(model().attributeExists("form"))
                .andExpect(model().attribute("isDiffMail",true))
                .andExpect(view().name("/team/addPlayer"))
                .andExpect(status().is2xxSuccessful());

        Mockito.verify(teamService,Mockito.times(1)).create(Mockito.any(CreateTeamModel.class));
    }

    @Test
    void testPostPlayerMethod() throws Exception{
        List<AddPlayerModel> list = new ArrayList<>();
        list.add(addPlayerModel1);

        Team team = new Team();
        team.setId(1);

        Mockito.when(addPlayerlistModel.getPlayers()).thenReturn(list);
        Mockito.when(addPlayerModel1.getTeamId()).thenReturn(team.getId());
        Mockito.when(teamRepository.findById(Mockito.anyInt())).thenReturn(team);
        Mockito.when(addPlayerModel1.addPlayer()).thenCallRealMethod();
        Mockito.when(addPlayerModel1.getMail()).thenReturn("test@test.fr");

        mockMvc.perform(post("/team/addPlayer")
                        .flashAttr("form",addPlayerlistModel))
                .andDo(print())
                .andExpect(view().name("redirect:/team/create"))
                .andExpect(redirectedUrl("/team/create"))
                .andExpect(status().is3xxRedirection());

        Mockito.verify(addPlayerlistModel,Mockito.times(1)).getPlayers();
        Mockito.verify(addPlayerModel1,Mockito.times(1)).getTeamId();
        Mockito.verify(addPlayerModel1,Mockito.times(1)).addPlayer();
        Mockito.verify(addPlayerModel1,Mockito.times(1)).getMail();
        Mockito.verify(teamRepository,Mockito.times(1)).findById(Mockito.anyInt());

    }

    @Test
    void testPostSamePlayer() throws Exception {
        List<AddPlayerModel> list = new ArrayList<>();
        list.add(addPlayerModel1);
        list.add(addPlayerModel2);

        Team team = new Team();
        team.setId(1);

        Mockito.when(addPlayerlistModel.getPlayers()).thenReturn(list);
        Mockito.when(addPlayerModel1.getTeamId()).thenReturn(team.getId());
        Mockito.when(teamRepository.findById(Mockito.anyInt())).thenReturn(team);
        Mockito.when(addPlayerModel1.addPlayer()).thenCallRealMethod();
        Mockito.when(addPlayerModel1.getMail()).thenReturn("test@test.fr");
        Mockito.when(addPlayerModel2.addPlayer()).thenCallRealMethod();
        Mockito.when(addPlayerModel2.getMail()).thenReturn("test@test.fr");

        mockMvc.perform(post("/team/addPlayer/")
                        .flashAttr("form", addPlayerlistModel))
                .andDo(print())
                .andExpect(view().name("/team/addPlayer"))
                .andExpect(status().is2xxSuccessful());

        //Mockito.verify(addPlayerlistModel,Mockito.times(1)).getPlayers();
        Mockito.verify(addPlayerModel1,Mockito.times(1)).getTeamId();
        Mockito.verify(addPlayerModel1,Mockito.times(1)).addPlayer();
        Mockito.verify(addPlayerModel1,Mockito.times(3)).getMail();
        Mockito.verify(teamRepository,Mockito.times(1)).findById(Mockito.anyInt());
        Mockito.verify(addPlayerModel2,Mockito.times(1)).addPlayer();
        Mockito.verify(addPlayerModel2,Mockito.times(3)).getMail();
    }
}

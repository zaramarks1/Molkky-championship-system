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
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


@WebMvcTest(value = TeamController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class TeamCreateControllerTest {
    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private TeamRepository teamRepository;

    @MockBean
    private TeamService teamService;

    @Autowired
    private TeamController teamController;

    @MockBean
    private EmailSenderService emailSenderService;

    /*
    Test work with conditions, example : @Controller -> @RestController or add @RequestBody to method
      but then the page is not return

     */
    @Test
    void testTeamGetMethod() throws Exception{
        this.mockMvc.perform(get("/team/create/")).andExpect(status().isOk());
    }


    @Test
    void testPostTeamMethod() throws Exception{
        Tournament tournament = new Tournament();
        tournament.setNbPlayersPerTeam(2);

        Team team = new Team();
        team.setTournament(tournament);

        when(teamService.create(any(CreateTeamModel.class))).thenReturn(team);

        mockMvc.perform(post("/team/create")
                .param("nom","Test")
                .param("tournamentId","1")
                .flashAttr("teamModel",new CreateTeamModel()))
                .andDo(MockMvcResultHandlers.print())
                .andExpect(view().name("/team/addPlayer"))
                .andExpect(model().attribute("team",team))
                .andExpect(model().attributeExists("teamModel"))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void testPostPlayerMethod() throws Exception{
        List<AddPlayerModel> list = new ArrayList<>();
        AddPlayerModel addPlayerModel1 = mock(AddPlayerModel.class);
        AddPlayerlistModel addPlayerlistModel = mock(AddPlayerlistModel.class);
        list.add(addPlayerModel1);

        Team team = new Team();
        team.setId(1);

        when(addPlayerlistModel.getPlayers()).thenReturn(list);
        when(addPlayerModel1.getTeamId()).thenReturn(team.getId());
        when(teamRepository.findById(anyInt())).thenReturn(team);
        when(addPlayerModel1.addPlayer()).thenCallRealMethod();
        when(addPlayerModel1.getMail()).thenReturn("test@test.fr");

        mockMvc.perform(post("/team/addPlayer")
                        .flashAttr("form",addPlayerlistModel))
                .andDo(MockMvcResultHandlers.print())
                .andExpect(view().name("redirect:/team/create"))
                .andExpect(status().is3xxRedirection());

    }

    @Test
    void testPostSamePlayer() throws Exception {
        List<AddPlayerModel> list = new ArrayList<>();
        AddPlayerModel addPlayerModel1 = mock(AddPlayerModel.class);
        AddPlayerModel addPlayerModel2 = mock(AddPlayerModel.class);
        AddPlayerlistModel addPlayerlistModel = mock(AddPlayerlistModel.class);

        list.add(addPlayerModel1);
        list.add(addPlayerModel2);

        Team team = new Team();
        team.setId(1);

        when(addPlayerlistModel.getPlayers()).thenReturn(list);
        when(addPlayerModel1.getTeamId()).thenReturn(team.getId());
        when(teamRepository.findById(anyInt())).thenReturn(team);
        when(addPlayerModel1.addPlayer()).thenCallRealMethod();
        when(addPlayerModel1.getMail()).thenReturn("test@test.fr");
        when(addPlayerModel2.addPlayer()).thenCallRealMethod();
        when(addPlayerModel2.getMail()).thenReturn("test@test.fr");

        mockMvc.perform(post("/team/addPlayer/")
                        .flashAttr("form", addPlayerlistModel))
                .andDo(MockMvcResultHandlers.print())
                .andExpect(view().name("/team/addPlayer"))
                .andExpect(status().is2xxSuccessful());
    }
}

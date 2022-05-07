package com.molkky.molkky.controllers;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.UserTounamentRole;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.TeamService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.List;

import static java.lang.reflect.Modifier.TRANSIENT;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@RunWith(SpringJUnit4ClassRunner.class)
public class TeamCreateControllerTest {

    private MockMvc mockMvc;

    @Mock
    private TournamentRepository tournamentRepository;

    @Mock
    private TeamRepository teamRepository;

    @Mock
    private AddPlayerlistModel addPlayerlistModel;

    @Mock
    private AddPlayerModel addPlayerModel1;

    @Mock
    private AddPlayerModel addPlayerModel2;

    @Mock
    private TeamService teamService;

    @InjectMocks
    private TeamController teamController;

    @Before
    public void setUp() throws Exception {
        mockMvc = MockMvcBuilders.standaloneSetup(teamController).build();
        MockitoAnnotations.openMocks(this);
    }
    /*
    Test work with conditions, example : @Controller -> @RestController or add @RequestBody to method
      but then the page is not return w
    @Test
    public void testTeamGetMethod() throws Exception{
        this.mockMvc.perform(get("/team/create"));
    }
    */

    @Test
    public void testPostTeamMethod() throws Exception{
        Tournament tournament = new Tournament();
        tournament.setNbPlayersPerTeam(2);

        Team team = new Team();
        team.setTournament(tournament);

        Mockito.when(teamService.create(Mockito.any(CreateTeamModel.class))).thenReturn(team);

        mockMvc.perform(post("/team/create")
                .param("nom","Test")
                .param("tournamentId","1")
                .flashAttr("teamModel",new CreateTeamModel()))
                .andDo(MockMvcResultHandlers.print())
                .andExpect(view().name("/team/addPlayer"))
                .andExpect(model().attribute("team",team))
                .andExpect(model().attributeExists("teamModel"))
                .andExpect(MockMvcResultMatchers.forwardedUrl("/team/addPlayer"))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    public void testPostPlayerMethod() throws Exception{
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
                .andDo(MockMvcResultHandlers.print())
                .andExpect(view().name("redirect:/team/create"))
                .andExpect(MockMvcResultMatchers.redirectedUrl("/team/create"))
                .andExpect(status().is3xxRedirection());

    }
    /*Same issue as GetTest
    @Test
    public void testPostSamePlayer() throws Exception{
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

        mockMvc.perform(post("/team/addPlayer")
                        .flashAttr("form",addPlayerlistModel))
                .andDo(MockMvcResultHandlers.print())
                .andExpect(view().name("/team/addPlayer"))
                .andExpect(MockMvcResultMatchers.forwardedUrl("/team/create"))
                .andExpect(status().is2xxSuccessful());
    }
     */
}

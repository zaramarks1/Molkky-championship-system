package com.molkky.molkky.controllers;

import com.molkky.molkky.repository.UserRepository;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringJUnit4ClassRunner.class)
public class ConnexionControllerTest {
    private MockMvc mockMvc;

    @InjectMocks
    private ConnexionController connexionController;

    @Mock
    private UserRepository userRepository;

    @Before
    public void setUp() throws Exception {
        mockMvc = MockMvcBuilders.standaloneSetup(connexionController).build();
    }

    @Test
    public void testConnexionControllerWithInexistantUser() throws Exception {
        mockMvc.perform(get("/connexion/"))
                .andExpect(status().isOk());
        mockMvc.perform(post("/connexion/")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"email\": \"paco@sfr.fr\", \"password\": \"test\", \"code\": \"test\"}")
        )
                .andExpect(status().is3xxRedirection());
    }

    @Test
    public void testConnexionControllerWithExistantUser() throws Exception {
        mockMvc.perform(get("/connexion/"))
                .andExpect(status().isOk());
        mockMvc.perform(post("/connexion/")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"email\": \"zara.marks@reseau.eseo.fr\", \"password\": \"test\", \"code\": \"test\"}")
                )
                .andExpect(status().is3xxRedirection());
    }
}

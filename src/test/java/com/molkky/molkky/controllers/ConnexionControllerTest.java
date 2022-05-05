package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserConnectionModel;
import com.molkky.molkky.repository.UserRepository;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringJUnit4ClassRunner.class)
public class ConnexionControllerTest {
    private MockMvc mockMvc;

    @InjectMocks
    private ConnexionController connexionController;

    @Mock
    private UserRepository userRepository;

    @Mock
    private UserConnectionModel userModel;

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
        User user = new User();
        user.setEmail("test" + Math.random() * 10000 + "@gmail.com");
        user.setPassword("Test2");
        this.userRepository.save(user);

        mockMvc.perform(get("/connexion/"))
                .andExpect(status().isOk());
        mockMvc.perform(post("/connexion/")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"email\":\"" + user.getEmail() + "\", \"password\":\"" + user.getPassword() + "\", \"code\": \"test\"}")
                )
                .andExpect(status().is3xxRedirection());
    }
}

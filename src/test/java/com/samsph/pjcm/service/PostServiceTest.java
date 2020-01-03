package com.samsph.pjcm.service;

import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.dao.JournalRepository;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.dao.PostReviewerRepository;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.query.JournalQuery;
import com.samsph.pjcm.query.PostQuery;
import com.samsph.pjcm.query.PostReviewerQuery;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.test.context.junit4.SpringRunner;

import javax.annotation.Resource;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static com.samsph.pjcm.config.DevUserId.*;
import static org.hamcrest.CoreMatchers.is;

@SpringBootTest
@RunWith(SpringRunner.class)
class PostServiceTest {
    @Resource
    private PostService postService;

    @Resource
    private PostRepository postRepository;

    private Post post;

    @Resource
    PostReviewerService postReviewerService;

    @Resource
    PostReviewerRepository postReviewerRepository;

    @Resource
    JournalService journalService;

    @Resource
    JournalRepository journalRepository;


    private Date start = new Date(2019, Calendar.DECEMBER, 10);
    private Date end = new Date(2019, Calendar.DECEMBER, 12);
    private Date date = new Date(2019, Calendar.DECEMBER, 11);
    private Date date2 = new Date(2019, Calendar.DECEMBER, 13);
    List<Integer> list;

    @BeforeEach
    void setUp() {
        list = new ArrayList<Integer>();
        PostQuery postSaveQuery = new PostQuery();
        postSaveQuery.setField(Field.BASIC_MEDICINE);
        postSaveQuery.setTitle("论文标题");
        postSaveQuery.setGenre(Genre.PAPER);
        postSaveQuery.setFundLevel(FundLevel.MUNICIPAL);
        postSaveQuery.setWritersInfo("张三;李四");

        post = postService.savePost(postSaveQuery, CONTRIBUTOR_ID);
    }

    @AfterEach
    void tearDown() {
        postRepository.deleteAll();
        postReviewerRepository.deleteAll();
        journalRepository.deleteAll();
    }

    @Test
    void savePost() {
        Assert.assertThat(post.getField(), is(Field.BASIC_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论文标题"));
        Assert.assertThat(post.getGenre(), is(Genre.PAPER.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.MUNICIPAL.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("张三;李四"));
        Assert.assertThat(post.getCount(), is(0));
        Assert.assertThat(post.getInvoiceNeeded(), is(MyBoolean.DEFAULT.getCode()));
        Assert.assertThat(post.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));
    }

    @Test
    void getPost() {
        post = postService.getPost(post.getId());
        Assert.assertThat(post.getField(), is(Field.BASIC_MEDICINE.getCode()));
        Assert.assertThat(post.getTitle(), is("论文标题"));
        Assert.assertThat(post.getGenre(), is(Genre.PAPER.getCode()));
        Assert.assertThat(post.getFundLevel(), is(FundLevel.MUNICIPAL.getCode()));
        Assert.assertThat(post.getWritersInfo(), is("张三;李四"));
        Assert.assertThat(post.getCount(), is(0));
        Assert.assertThat(post.getInvoiceNeeded(), is(MyBoolean.DEFAULT.getCode()));
        Assert.assertThat(post.getStatus(), is(PostStatus.TO_BE_SUBMITTED.getCode()));
        Assert.assertThat(post.getContributorUid(), is(CONTRIBUTOR_ID));

        try {
            post = postService.getPost(0);
        } catch (CustomException e) {
            Assert.assertThat(e.getMessage(), is("稿件未找到"));
            Assert.assertThat(e.getCode(), is(400));
        }
    }

    @Test
    void updatePost() {
        post.setStatus(PostStatus.SUCCESS.getCode());
        postService.updatePost(post);
        post = postService.getPost(post.getId());
        Assert.assertThat(post.getStatus(), is(PostStatus.SUCCESS.getCode()));
    }

    @Test
    void getAllByCtrUid() {
        Page<Post> posts = postService.getAllByCtrUid(CONTRIBUTOR_ID + 1, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        posts = postService.getAllByCtrUid(CONTRIBUTOR_ID, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByCtrUidAndSubmitTime() {
        Page<Post> posts = postService.getAllByCtrUidAndSubmitTime(CONTRIBUTOR_ID, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setSubmitTime(date);
        postService.updatePost(post);

        posts = postService.getAllByCtrUidAndSubmitTime(CONTRIBUTOR_ID, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

//    @Test
//    void getAllByCtrUidAndStatus() {
//        Page<Post> posts = postService.getAllByCtrUidAndStatus(CONTRIBUTOR_ID, PostStatus.PENDING_FIRST_EXAM, 1, 5, true);
//        Assert.assertThat(posts.getNumberOfElements(), is(0));
//
//        posts = postService.getAllByCtrUidAndStatus(CONTRIBUTOR_ID, PostStatus.TO_BE_SUBMITTED, 1, 5, true);
//        Assert.assertThat(posts.getNumberOfElements(), is(1));
//        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
//    }

//    @Test
//    void getAllByCtrUidAndStatusAndSubmitTime() {
//        post.setSubmitTime(date2);
//        postService.updatePost(post);
//
//        Page<Post> posts = postService.getAllByCtrUidAndStatusAndSubmitTime(CONTRIBUTOR_ID, PostStatus.TO_BE_SUBMITTED, start, end, 1, 5, true);
//        Assert.assertThat(posts.getNumberOfElements(), is(0));
//
//        post.setSubmitTime(date);
//        postService.updatePost(post);
//
//        posts = postService.getAllByCtrUidAndStatusAndSubmitTime(CONTRIBUTOR_ID, PostStatus.SUCCESS, start, end, 1, 5, true);
//        Assert.assertThat(posts.getNumberOfElements(), is(0));
//
//        posts = postService.getAllByCtrUidAndStatusAndSubmitTime(CONTRIBUTOR_ID, PostStatus.TO_BE_SUBMITTED, start, end, 1, 5, true);
//        Assert.assertThat(posts.getNumberOfElements(), is(1));
//        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
//    }

    @Test
    void getAllByEdUid() {
        Page<Post> posts = postService.getAllByEdUid(EDITOR_ID, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setEditorUid(EDITOR_ID);
        postService.updatePost(post);

        posts = postService.getAllByEdUid(EDITOR_ID, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByEdUidAndSubmitTime() {
        post.setEditorUid(EDITOR_ID);
        postService.updatePost(post);

        Page<Post> posts = postService.getAllByEdUidAndSubmitTime(EDITOR_ID, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setSubmitTime(start);
        postService.updatePost(post);
        posts = postService.getAllByEdUidAndSubmitTime(EDITOR_ID, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setSubmitTime(date);
        postService.updatePost(post);

        posts = postService.getAllByEdUidAndSubmitTime(EDITOR_ID, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByEdUidAndStatus() {
        post.setEditorUid(EDITOR_ID);
        postService.updatePost(post);

        list.add(PostStatus.SUCCESS.getCode());
        Page<Post> posts = postService.getAllByEdUidAndStatus(EDITOR_ID, list, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setStatus(PostStatus.SUCCESS.getCode());
        postService.updatePost(post);

        posts = postService.getAllByEdUid(EDITOR_ID, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByEdUidAndStatusAndSubmitTime() {
        post.setEditorUid(EDITOR_ID);
        post.setStatus(PostStatus.SUCCESS.getCode());
        post.setSubmitTime(end);
        postService.updatePost(post);
        Page<Post> posts = postService.getAllByEdUidAndStatusAndSubmitTime(EDITOR_ID, list, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setSubmitTime(date);
        postService.updatePost(post);
        posts = postService.getAllByEdUid(EDITOR_ID, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByRevUidAndAccept() {
        Page<Post> posts = postService.getAllByRevUidAndAccept(REVIEWER_ID, MyBoolean.DEFAULT, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        postReviewerService.save(new PostReviewerQuery(post.getId(), REVIEWER_ID, null));
        posts = postService.getAllByRevUidAndAccept(REVIEWER_ID, MyBoolean.DEFAULT, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
        posts = postService.getAllByRevUidAndAccept(REVIEWER_ID + 1, MyBoolean.DEFAULT, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));
        posts = postService.getAllByRevUidAndAccept(REVIEWER_ID, MyBoolean.TRUE, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));
    }

    @Test
    void getAllByRevUidAndAcceptAndSubmitTime() {
        postReviewerService.save(new PostReviewerQuery(post.getId(), REVIEWER_ID, null));
        Page<Post> posts = postService.getAllByRevUidAndAcceptAndSubmitTime(REVIEWER_ID, MyBoolean.DEFAULT, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setSubmitTime(date);
        postService.updatePost(post);
        posts = postService.getAllByRevUidAndAcceptAndSubmitTime(REVIEWER_ID, MyBoolean.DEFAULT, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByRevUidAndFlag() {
        Page<Post> posts = postService.getAllByRevUidAndFlag(REVIEWER_ID, true, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        PostReviewer postReviewer = postReviewerService.save(new PostReviewerQuery(post.getId(), REVIEWER_ID, null));
        posts = postService.getAllByRevUidAndFlag(REVIEWER_ID, false, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        postReviewer.setAccept(MyBoolean.TRUE.getCode());
        postReviewerService.updatePostReviewer(postReviewer);
        posts = postService.getAllByRevUidAndFlag(REVIEWER_ID, false, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));

        posts = postService.getAllByRevUidAndFlag(REVIEWER_ID, true, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        postReviewer.setFlag(true);
        postReviewerService.updatePostReviewer(postReviewer);
        posts = postService.getAllByRevUidAndFlag(REVIEWER_ID, true, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByRevUidAndFlagAndSubmitTime() {
        PostReviewer postReviewer = postReviewerService.save(new PostReviewerQuery(post.getId(), REVIEWER_ID, null));
        postReviewer.setAccept(MyBoolean.TRUE.getCode());
        postReviewerService.updatePostReviewer(postReviewer);

        Page<Post> posts = postService.getAllByRevUidAndFlagAndSubmitTime(REVIEWER_ID, false, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setSubmitTime(date2);
        postService.updatePost(post);
        posts = postService.getAllByRevUidAndFlagAndSubmitTime(REVIEWER_ID, false, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setSubmitTime(date);
        postService.updatePost(post);
        posts = postService.getAllByRevUidAndFlagAndSubmitTime(REVIEWER_ID, false, start, end, 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void getAllByJid() {
        Journal journal = journalService.saveJournal(new JournalQuery(2019, 11, 11, 1, 1,""), ADMIN_ID);
        Page<Post> posts = postService.getAllByJid(journal.getId(), 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(0));

        post.setJid(journal.getId());
        postService.updatePost(post);
        posts = postService.getAllByJid(journal.getId(), 1, 5, true);
        Assert.assertThat(posts.getNumberOfElements(), is(1));
        Assert.assertThat(posts.getContent().get(0).getId(), is(post.getId()));
    }

    @Test
    void deletePost() {
        postService.deletePost(post.getId());
        try {
            post = postService.getPost(post.getId());
        } catch (CustomException e) {
            Assert.assertThat(e.getMessage(), is("稿件未找到"));
            Assert.assertThat(e.getCode(), is(400));
        }
        try {
            postService.deletePost(post.getId());
        } catch (CustomException e) {
            Assert.assertThat(e.getMessage(), is("稿件未找到"));
            Assert.assertThat(e.getCode(), is(400));
        }
    }
}
package com.samsph.pjcm.service;

import com.samsph.pjcm.config.constant.Field;
import com.samsph.pjcm.config.constant.FundLevel;
import com.samsph.pjcm.config.constant.Genre;
import com.samsph.pjcm.config.constant.MyBoolean;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.dao.JournalRepository;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.dao.PostReviewerRepository;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.query.PostQuery;
import com.samsph.pjcm.query.PostReviewerQuery;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import javax.annotation.Resource;

import static com.samsph.pjcm.config.DevUserId.CONTRIBUTOR_ID;
import static com.samsph.pjcm.config.DevUserId.REVIEWER_ID;
import static org.hamcrest.CoreMatchers.is;

@SpringBootTest
@RunWith(SpringRunner.class)
class PostReviewerServiceTest {
    @Resource
    private PostService postService;

    @Resource
    private PostReviewerService postReviewerService;

    @Resource
    private PostRepository postRepository;

    @Resource
    private PostReviewerRepository postReviewerRepository;

    private Post post;
    private PostReviewer postReviewer;
    private PostReviewerQuery postReviewerQuery;

    @BeforeEach
    void setUp() {
        PostQuery postSaveQuery = new PostQuery();
        postSaveQuery.setField(Field.BASIC_MEDICINE);
        postSaveQuery.setTitle("论文标题");
        postSaveQuery.setGenre(Genre.PAPER);
        postSaveQuery.setFundLevel(FundLevel.MUNICIPAL);
        postSaveQuery.setWritersInfo("张三;李四");

        post = postService.savePost(postSaveQuery, CONTRIBUTOR_ID);

        postReviewerQuery = new PostReviewerQuery(post.getId(), REVIEWER_ID, null);
        postReviewer = postReviewerService.save(postReviewerQuery);
    }

    @AfterEach
    void tearDown() {
        postRepository.deleteAll();
        ;
        postReviewerRepository.deleteAll();
    }

    @Test
    void save() {
        Assert.assertThat(postReviewer.getAccepted(), is(MyBoolean.DEFAULT.getCode()));
        Assert.assertThat(postReviewer.getFlag(), is(false));
        Assert.assertThat(postReviewer.getPid(), is(post.getId()));
        Assert.assertThat(postReviewer.getReviewerUid(), is(REVIEWER_ID));

        try {
            postReviewerService.save(postReviewerQuery);
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("稿件已选择该审稿人"));
        }
    }

    @Test
    void getPostReviewer() {
        PostReviewer postReviewer2 = postReviewerService.getPostReviewer(post.getId(), REVIEWER_ID);
        Assert.assertThat(postReviewer2.getAccepted(), is(MyBoolean.DEFAULT.getCode()));
        Assert.assertThat(postReviewer2.getFlag(), is(false));
        Assert.assertThat(postReviewer2.getPid(), is(post.getId()));
        Assert.assertThat(postReviewer2.getReviewerUid(), is(REVIEWER_ID));

        try {
            postReviewerService.getPostReviewer(0, REVIEWER_ID);
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("稿件-审稿人记录未找到"));
        }
    }

    @Test
    void updatePostReviewer() {
        postReviewer.setFlag(true);
        postReviewer.setAccepted(MyBoolean.TRUE.getCode());
        postReviewerService.updatePostReviewer(postReviewer);

        Assert.assertThat(postReviewer.getAccepted(), is(MyBoolean.TRUE.getCode()));
        Assert.assertThat(postReviewer.getFlag(), is(true));
        Assert.assertThat(postReviewer.getPid(), is(post.getId()));
        Assert.assertThat(postReviewer.getReviewerUid(), is(REVIEWER_ID));
    }

    @Test
    void aggregate() {
        Assert.assertThat(postReviewerService.aggregate(post.getId()), is(true));

        postReviewer.setFlag(true);
        postReviewerService.updatePostReviewer(postReviewer);
        Assert.assertThat(postReviewerService.aggregate(post.getId()), is(false));
    }

    @Test
    void deletePostReviewer() {
        postReviewerService.deletePostReviewer(postReviewer.getId());

        try {
            postReviewerService.getPostReviewer(post.getId(),REVIEWER_ID);
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("稿件-审稿人记录未找到"));
        }
    }
}